#![allow(warnings)]
mod buffer;
mod regex;

use std::io;
use std::io::{BufReader,BufRead,BufWriter,Write};
use std::fs::File;
use std::fmt::{Display, Formatter};

use self::buffer::{Address, Buffer};

use libc::{c_ushort, ioctl, STDOUT_FILENO, TIOCGWINSZ};

const FILENAME_MAX_LENGTH: usize = 64;

#[derive(Default,Clone)]
struct Cli {
    buffer: Buffer,
    default_filename: Option<String>,
    print_prompt: bool,
    prompt: String,
    last_error: Option<Error>,
    dirty: bool,
    warned: bool,
    explain_errors: bool,
    saved_state: Option<Box<Self>>,
    last_command: Option<String>,
    last_pattern: Option<Vec<u8>>,
    last_substitution: Option<Command>,
}

impl Cli {
    fn new() -> Self {
        Self {
            prompt: "*".to_string(),
            ..Default::default()
        }
    }

    fn read_command(&self) -> Result<String,Error> {
        let mut line = String::new();
        if self.print_prompt {
            print!("{}",self.prompt);
        }
        if io::stdout().flush().is_err() {
            return Err(Error::CannotFlushStdout);
        }
        if io::stdin().read_line(&mut line).is_err() {
            return Err(Error::CannotReadStdin);
        }
        Ok(line.trim().to_string())
    }

    fn parse_address_token(&mut self, command: &mut String)
                           -> Option<Result<AddressToken, Error>> {
        if command.is_empty() { return None; }
        let mut chars = command.as_bytes().iter().peekable();
        match chars.peek() {
            Some(&b'.') => {
                let _ = command.drain(..1);
                Some(Ok(AddressToken::Dot))
            },
            Some(&b'$') => {
                let _ = command.drain(..1);
                Some(Ok(AddressToken::Dollar))
            },
            Some(&b'-') => {
                let _ = command.drain(..1);
                Some(Ok(AddressToken::Minus))
            },
            Some(&b'+') => { 
                let _ = command.drain(..1);
                Some(Ok(AddressToken::Plus)) 
            },
            Some(&b',') => { 
                let _ = command.drain(..1);
                Some(Ok(AddressToken::Comma)) 
            },
            Some(&b'%') => {
                let _ = command.drain(..1);
                Some(Ok(AddressToken::Percent)) 
            },
            Some(&b'/') => {
                if command.len() == 1 {
                    if let Some(pattern) = self.last_pattern.clone() {
                        command.clear();
                        return Some(Ok(AddressToken::ForwardSearch(pattern)));
                    } else {
                        return Some(Err(Error::NoPreviousPattern));
                    }
                }
                // Go forward and gather until next / not preceded by \
                let mut was_back_slash = false;
                let mut res = Vec::new();
                for c in chars.skip(1) {
                    if *c == b'/' && !was_back_slash {
                        let _ = command.drain(..2+res.len());
                        self.last_pattern = Some(res.clone());
                        return Some(Ok(AddressToken::ForwardSearch(res)));
                    } else if *c == b'\\' {
                        if was_back_slash {
                            was_back_slash = false;
                            res.push(b'\\');
                        } else {
                            was_back_slash = true;
                        }
                    } else {
                        if was_back_slash {
                            res.push(b'\\');
                            was_back_slash = false;
                        }
                        res.push(*c);
                    }
                }

                command.clear();
                self.last_pattern = Some(res.clone());
                Some(Ok(AddressToken::ForwardSearch(res)))
            },
            Some(&b'\'') => {
                chars.next();
                if let Some(&c) = chars.next() {
                    if !c.is_ascii_lowercase() {
                        return Some(Err(Error::InvalidMarkCharacter));
                    }
                    let _ = command.drain(..2);
                    Some(Ok(AddressToken::Mark(c)))
                } else {
                    Some(Err(Error::InvalidAddress))
                }
            },
            Some(&b';') => {
                let _ = command.drain(..1);
                Some(Ok(AddressToken::Semicolon))
            },
            Some(&b'^') => {
                let _ = command.drain(..1);
                Some(Ok(AddressToken::Caret))
            },
            Some(&b'?') => {
                if command.len() == 1 {
                    if let Some(pattern) = self.last_pattern.clone() {
                        command.clear();
                        return Some(Ok(AddressToken::BackwardSearch(pattern)));
                    } else {
                        return Some(Err(Error::NoPreviousPattern));
                    }
                }
                // go forward and gather until next / not preceded by \
                let mut was_back_slash = false;
                let mut res = Vec::new();
                for c in chars.skip(1) {
                    if *c == b'?' && !was_back_slash {
                        let _ = command.drain(..2+res.len());
                        self.last_pattern = Some(res.clone());
                        return Some(Ok(AddressToken::BackwardSearch(res)));
                    } else if *c == b'\\' {
                        if was_back_slash {
                            was_back_slash = false;
                            res.push(b'\\');
                        } else {
                            was_back_slash = true;
                        }
                    } else {
                        if was_back_slash {
                            res.push(b'\\');
                            was_back_slash = false;
                        }
                        res.push(*c);
                    }
                }

                command.clear();
                self.last_pattern = Some(res.clone());
                Some(Ok(AddressToken::BackwardSearch(res)))
            },
            Some(x) if x.is_ascii_digit() => {
                let (num,count) = chars.take_while(|x| x.is_ascii_digit())
                    .fold((0usize,0),|(acc,count),e| 
                        ((10 * acc) + (e - b'0') as usize, count + 1));
                let _ = command.drain(..count);
                Some(Ok(AddressToken::Number(num)))
            },
            _ => None,
        }
    }

    fn eval_address_expr(&self, tokens: &[AddressToken]) 
        -> Result<Option<usize>,Error> {
        use AddressToken::*;
        match tokens {
            [Plus, Number(n)] => {
                return Ok(Some(self.buffer.plus(*n)?));
            },
            [Minus, Number(n)] => {
                return Ok(Some(self.buffer.minus(*n)?));
            },
            [Plus] => {
                return Ok(Some(self.buffer.plus(1)?));
            },
            [Minus] => {
                return Ok(Some(self.buffer.minus(1)?));
            },
            _ => (),
        };

        let mut slice = tokens;

        let mut total: Option<isize> = None;

        slice = match slice {
            [Number(n), rest @ ..] => {
                total = Some(*n as isize);
                rest
            },
            [Dot, rest @ ..] => {
                total = Some(self.buffer.cur()? as isize);
                rest
            },
            [Dollar, rest @ ..] => {
                total = Some(self.buffer.end()? as isize);
                rest
            },
            [ForwardSearch(pattern), rest @ ..] => {
                total = Some(self.buffer.forward_search(pattern)? as isize);
                rest
            },
            [BackwardSearch(pattern), rest @ ..] => {
                total = Some(self.buffer.backward_search(pattern)? as isize);
                rest
            },
            [Mark(c), rest @ ..] => {
                total = Some(self.buffer.get_mark(*c)?
                    .ok_or(Error::InvalidAddress)? as isize);
                rest
            },
            _ => slice,
        };

        while !slice.is_empty() {
            slice = match slice {
                [Plus, Number(n), rest @ ..] => {
                    total = total.take().map(|x| x + *n as isize);
                    rest
                },
                [Minus, Number(n), rest @ ..] 
                    | [Caret, Number(n), rest @ ..] => {
                    total = total.take().map(|x| x - *n as isize);
                    rest
                },
                _ => return Err(Error::InvalidAddress),
            }
        }
        match total {
            Some(x) if x > 0 => Ok(Some(x as usize)),
            Some(x) => Err(Error::InvalidAddress),
            _ => Ok(None),
        }
    }

    fn parse_address(&mut self, command: String) 
        -> Result<(Address, String), Error> {
        use AddressToken::*;

        let mut command = command;

        let mut tokens = vec![];

        while let Some(tok) = self.parse_address_token(&mut command) {
            tokens.push(tok?);
        }

        let command = command.trim().to_string();

        match &tokens[..] {
            [] => return Ok((Address::Default, command)),
            [Percent] | [Comma] => {
                return Ok((self.buffer.all()?, command));
            },
            [Semicolon] => { 
                return Ok((Address::Range(self.buffer.cur()?,
                    self.buffer.end()?), command));
            }
            _ => (),
        }

        let mut tail = match tokens.iter().position(|t| *t == Comma) {
            Some(idx) => {
                let res = tokens.split_off(idx + 1);
                tokens.pop(); // remove Comma
                res
            },
            _ => if let Some(x) = self.eval_address_expr(&tokens[..])? {
                return Ok((Address::Line(x),command));
            } else {
                return Err(Error::InvalidAddress);
            }
        };

        let addr = match (self.eval_address_expr(&tokens[..])?, 
            self.eval_address_expr(&tail[..])?) {
            (Some(x),Some(y)) => Address::Range(x,y),
            (Some(x),None) => Address::Line(x),
            (None,Some(x)) => Address::Range(self.buffer.cur()?,x),
            _ => return Err(Error::InvalidAddress),
        };

        Ok((addr, command))
    }

    fn parse_command(&mut self, line: String) -> Result<Command, Error> {
        let (addr, line) = self.parse_address(line)?;

        match &line[..] {
            "" => {
                if addr == Address::Default {
                    return Ok(Command::Print(Address::Line(self.buffer.cur()? + 1)));
                } else {
                    return Ok(Command::PrintOne(addr));
                }
            },
            "c" => return Ok(Command::Change(addr)),
            "d" => return Ok(Command::Delete(addr)),
            "j" => return Ok(Command::Join(addr)),
            "p" => return Ok(Command::Print(addr)),
            "l" => return Ok(Command::PrintUnambiguous(addr)),
            "=" => return Ok(Command::PrintNumber(addr)),
            "n" => return Ok(Command::Number(addr)),
            "P" => return Ok(Command::Prompt),
            "a" => return Ok(Command::Append(addr)),
            "i" => return Ok(Command::Insert(addr)),
            "h" => return Ok(Command::Help),
            "H" => return Ok(Command::ExplainErrors),
            "q" => return Ok(Command::Quit),
            "Q" => return Ok(Command::QuitUnconditional),
            _ => (),
        }

        if line.starts_with("!") {
            if addr != Address::Default {
                return Err(Error::UnexpectedAddress);
            } else {
                return Ok(Command::ShellCommand(String::from(&line[1..])));
            }
        }

        let command = line.chars()
            .take_while(|c| !c.is_whitespace()).collect::<String>();

        match &command[0..1] {
            "m" => {
                let (dest_addr, rest) = self.parse_address(command[1..].to_string())?;
                if !rest.is_empty() { return Err(Error::InvalidCommandSuffix); }
                return Ok(Command::Move(addr,dest_addr));
            },
            "t" => {
                let (dest_addr, rest) = self.parse_address(command[1..].to_string())?;
                if !rest.is_empty() { return Err(Error::InvalidCommandSuffix); }
                return Ok(Command::Transfer(addr,dest_addr));
            },
            "k" => {
                if command.len() != 2 {
                    return Err(Error::InvalidCommandSuffix);
                } else {
                    return Ok(Command::Mark(addr, command.as_bytes()[1]));
                }
            },
            "s" => {
                // Take next byte as search delimiter
                let mut bytes = command[1..].as_bytes();
                let delim = bytes[0];
                // TODO Check validity
                bytes = &bytes[1..];

                let mut split = bytes.split(|x| *x == delim);
                let pattern = if let Some(pattern) = split.next() {
                    pattern.to_vec()
                } else {
                    return Ok(self.last_substitution
                        .clone()
                        .ok_or(Error::NoPreviousPattern)?);
                };

                let replacement = if let Some(replacement) = split.next() {
                    let mut iter = replacement.iter();
                    let mut res = Vec::new();
                    let mut tmp = Vec::new();
                    while let Some(b) = iter.next() {
                        match b {
                            _ => unimplemented!(),
                        }
                    }
                    if !tmp.is_empty() {
                        res.push(ReplacementToken::Text(tmp));
                    }
                    res
                } else {
                    // default empty
                    vec![]
                };
                let suffix = match split.next() {
                    None => SubstitutionSuffix::First,
                    Some(&[b'g']) => SubstitutionSuffix::Global,
                    Some(x) if x[0].is_ascii_digit() => {
                        let n = String::from_utf8(x.to_vec())
                            .map_err(|_| Error::InvalidCommandSuffix)?
                            .parse()
                            .map_err(|_| Error::InvalidCommandSuffix)?;
                        SubstitutionSuffix::Number(n)
                    },
                    _ => return Err(Error::InvalidCommandSuffix),
                };

                let command = Command::Substitute(addr, pattern, replacement, suffix);
                self.last_substitution = Some(command.clone());

                return Ok(command);
            },
            _ => (),
        }

        let arg = line[command.len()..].chars()
            .skip_while(|c| c.is_whitespace())
            .collect::<String>();

        let arg = if arg.is_empty() {
            None
        } else if arg.len() > FILENAME_MAX_LENGTH {
            return Err(Error::FilenameTooLong);
        } else {
            Some(arg)
        };

        match &command[..] {
            "w" => Ok(Command::Write(addr,arg)),
            "e" => Ok(Command::Edit(arg)),
            "f" => Ok(Command::Filename(arg)),
            "E" => Ok(Command::EditUnconditional(arg)),
            "r" => Ok(Command::Read(addr,arg)),
            "W" => Ok(Command::WriteAppend(addr, arg)),
            "wq" => Ok(Command::WriteQuit(addr, arg)),
            _ => Err(Error::UnknownCommand),
        }
    }

    // (.)a Append text to the buffer after the addressed line. 
    // Enters input mode. Current address set to the last line entered.
    fn append(&mut self, addr: Address) -> Result<(), Error> {
        let lines = Self::input()?;
        self.buffer.append(addr, lines)?;
        self.mark_dirty();
        Ok(())
    }

    // (.,.)c Change lines in the buffer. The addressed lines are deleted from the buffer, and
    // text is appended in their place. Text is entered in input mode. The current address is set
    // to the last line entered.
    fn change(&mut self, addr: Address) -> Result<(), Error> {
        if !self.buffer.is_valid(addr) {
            return Err(Error::InvalidAddress);
        }
        let lines = Self::input()?;
        self.buffer.change(addr, lines)?;
        self.mark_dirty();
        Ok(())
    }

    // (.,.)d Delete the addressed lines from the buffer. If there is a line after the deleted
    // range, set the current address to that line. Otherwise, set it to the last line in the
    // buffer.
    fn delete(&mut self, addr: Address) -> Result<(), Error> {
        self.buffer.delete(addr)?;
        self.mark_dirty();
        Ok(())
    }

    // g

    // (.)i Insert text to the buffer before the addressed line. 
    // Enters input mode. Current address set to the last line entered.
    fn insert(&mut self, addr: Address) -> Result<(), Error> {
        let lines = Self::input()?;
        self.buffer.insert(addr, lines)?;
        self.mark_dirty();
        Ok(())
    }

    fn join(&mut self, addr: Address) -> Result<(), Error> {
        self.buffer.join(addr)?;
        self.mark_dirty();
        Ok(())
    }

    fn mark(&mut self, addr: Address, c: u8) -> Result<(), Error> {
        let line = match addr {
            Address::Default => {
                self.buffer.cur()?
            },
            Address::Line(x) | Address::Range(_,x) => {
                if x > 0 && x <= self.buffer.len() {
                    x
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
        };
        self.buffer.mark((c, line))
    }

    // (.,.)n
    fn number(&mut self, addr: Address) -> Result<(), Error> {
        let (start, end) = match addr {
            Address::Default => {
                let num = self.buffer.cur()?;
                (num, num)
            },
            Address::Line(num) => (num, num),
            Address::Range(x, y) => (x, y),
        };
        let lines = self.buffer.fetch_change_address(addr)?;
        let mut writer = BufWriter::new(io::stdout());
        for (i,line) in (start..=end).zip(lines.iter()) {
            if write!(&mut writer, "{i:<8}").is_err() 
                || writer.write(line).is_err()
                || writer.write(b"\n").is_err() {
                return Err(Error::CannotWriteToStdout);
            }
        }
        Ok(())
    }

    // (.,.)p
    fn print(&mut self, addr: Address) -> Result<(),Error> {
        let mut writer = BufWriter::new(io::stdout());
        for line in self.buffer.fetch_change_address(addr)? {
            if writer.write(line).is_err() || writer.write(b"\n").is_err() {
                return Err(Error::CannotWriteToStdout);
            }
        }
        Ok(())
    }

    fn print_one(&mut self, addr: Address) -> Result<(),Error> {
        match addr {
            Address::Default => unreachable!(),
            Address::Line(_) => self.print(addr),
            Address::Range(_, x) => self.print(Address::Line(x)),

        }
    }

    fn print_number(&self, addr: Address) -> Result<(), Error> {
        let num = match addr {
            Address::Default => self.buffer.cur()?,
            Address::Line(num) | Address::Range(_, num) => {
                if self.buffer.is_valid(addr) {
                    num
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
        };
        println!("{num}");
        Ok(())
    }

    fn get_screen_dims() -> Result<(usize,usize),Error> {
        struct TermSize {
            rows: c_ushort,
            cols: c_ushort,
            x: c_ushort,
            y: c_ushort
        }
        let mut ts = TermSize { rows: 0, cols: 0, x: 0, y: 0 };
        let res = unsafe {
            ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut ts)
        };
        if res == 0 {
            Ok((ts.rows as usize, ts.cols as usize))
        } else {
            Err(Error::CannotGetTerminalSize)
        }
    }

    fn encode(line: &[u8], width: usize) 
        -> Vec<u8> {
        let mut buf = Vec::new();
        let mut line_len = 0;
        let mut arr = Vec::with_capacity(4);
        for b in line {
            let token: &[u8] = match *b {
                7 => br"\a",
                8 => br"\b",
                9 => br"\t",
                10 => br"\n",
                11 => br"\v",
                12 => br"\f",
                13 => br"\r",
                27 => br"\e",
                x @ 32..127 => &[x],
                x => {
                    arr.clear();
                    write!(&mut arr, "\\{:03o}", x);
                    &arr
                }
            };
            if line_len + token.len() + 1 > width {
                buf.extend(&[b'\\',b'\n']);
                line_len = 0;
            }
            buf.extend(token);
            line_len += token.len();
        }
        let token = &[b'$',b'\n'];
        if line_len + token.len() + 1 > width {
            buf.extend(&[b'\\',b'\n']);
        }
        buf.extend(token);
        buf
    }

    //(.)l
    fn print_unambiguous(&mut self, addr: Address) -> Result<(), Error> {
        let right_margin = 4;
        let (rows, cols) = Self::get_screen_dims()?;
        let cols = match cols {
            x if x < 2 * right_margin => right_margin + 1,
            x => x - right_margin,
        };
        let mut writer = BufWriter::new(io::stdout());
        for line in self.buffer.fetch_change_address(addr)? {
            
            writer.write(&Self::encode(line, cols)[..])
                .map_err(|_| Error::CannotWriteToStdout)?;
        }
        Ok(())
    }

    // P
    fn toggle_print_prompt(&mut self) -> Result<(), Error> {
        self.print_prompt = !self.print_prompt;
        Ok(())
    }

    // (.,.)s/re/replacement
    // (.,.)s/re/replacement/g
    // (.,.)s/re/replacement/n
    // (.,.)s

    // u Undo. u is its own inverse.

    // (1,$)w file
    fn write(&mut self, addr: Address, filename: Option<String>) 
        -> Result<(), Error> {
        self.write_inner(addr, filename, WriteMode::Create)
    }

    // (1,$)W file
    fn write_append(&mut self, addr: Address, filename: Option<String>) 
        -> Result<(), Error> {
        self.write_inner(addr, filename, WriteMode::Append)
    }

    fn write_inner(&mut self, addr: Address, 
        filename: Option<String>, 
        mode: WriteMode) -> Result<(), Error> {
        let filename = if filename.is_some() {
            if self.default_filename.is_none() {
                self.default_filename = filename.clone();
            }
            filename.as_ref().unwrap()
        } else if self.default_filename.is_some() {
            self.default_filename.as_ref().unwrap()
        } else {
            return Err(Error::NoCurrentFilename);
        };

        let fh = match mode {
            WriteMode::Create => File::create(filename),
            WriteMode::Append => File::options().append(true).create(true).open(filename),
        };

        if let Ok(mut f) = fh {
            let addr = match addr {
                Address::Default => self.buffer.all()?,
                _ => addr,
            };
            let buf: Vec<u8> = self.buffer.fetch(addr)?
                .into_iter()
                .fold(vec![], |mut acc, e| { 
                    acc.extend(e); 
                    acc.push(b'\n'); 
                    acc 
            });
            match f.write(&buf[..]) {
                Ok(num) => {
                    self.unmark_dirty();
                    println!("{}", num);
                    Ok(())
                },
                Err(_) => Err(Error::CannotWriteFile),
            }
        } else {
            Err(Error::CannotOpenOutputFile)
        }
    }


    fn help(&self) -> Result<(), Error> {
        match self.last_error.as_ref() {
            Some(e) => eprintln!("{}", e),
            _ => (),
        }
        Ok(())
    }

    fn toggle_explain_errors(&mut self) -> Result<(), Error> {
        self.explain_errors = !self.explain_errors;
        Ok(())
    }

    fn input() -> Result<Vec<Vec<u8>>, Error> {
        let mut lines = vec![];
        let mut reader = BufReader::new(io::stdin());
        loop {
            let mut line = Vec::new();
            if reader.read_until(b'\n', &mut line).is_err() {
                return Err(Error::CannotReadStdin);
            }
            line.pop();
            match &line[..] {
                &[b'.'] => return Ok(lines),
                _ => lines.push(line),
            }
        }
    }

    fn try_quit(&mut self) -> Result<(), Error> {
        match (self.dirty, self.warned) {
            (true, false) => {
                self.warned = true;
                Err(Error::WarningFileModified)
            },
            (false, true) => unreachable!(),
            _ => Ok(()),
        }
    }

    fn edit(&mut self, filename: Option<String>) -> Result<(), Error> {
        let lines = match filename {
            Some(name) if name.starts_with("!") => {
                let (mut stdout, stderr) = self
                    .command_inner(String::from(&name[1..]))?;
                io::stderr().write_all(&stderr[..])
                    .map_err(|_| Error::CannotWriteToStderr)?;
                stdout.split(|b| *b == b'\n')
                    .map(|split| Vec::from(split))
                    .collect::<Vec<_>>()
            },
            Some(name) => {
                Self::read_inner(&name[..])?
            },
            None => {
                if self.default_filename.is_some() {
                    Self::read_inner(self.default_filename.as_ref().unwrap())?
                } else {
                    return Err(Error::NoCurrentFilename);
                }
            }
        };

        self.unmark_dirty();
        self.buffer.load(lines);
        Ok(())
    }

    // ($)r file 
    // Read file to after the addressed line. If file is not specified, then
    // the default filename is used.
    fn read(&mut self, addr: Address, filename: Option<String>) -> Result<(), Error> {

        let lines = match filename {
            Some(name) if name.starts_with("!") => {
                let (mut stdout, stderr) = self
                    .command_inner(String::from(&name[1..]))?;
                io::stderr().write_all(&stderr[..])
                    .map_err(|_| Error::CannotWriteToStderr)?;
                // assert_eq!(stdout.pop(), Some(b'\n')); // Remove final newline
                stdout.split(|b| *b == b'\n')
                    .map(|split| Vec::from(split))
                    .collect::<Vec<_>>()
            },
            Some(name) => {
                if self.default_filename.is_none() {
                    self.default_filename = Some(name.clone());
                }
                Self::read_inner(&name)?
            },
            None => {
                if self.default_filename.is_some() {
                    Self::read_inner(self.default_filename.as_ref().unwrap())?
                } else {
                    return Err(Error::NoCurrentFilename);
                }
            },
        };

        self.buffer.append(addr, lines)
    }

    fn read_inner(filename: &str) -> Result<Vec<Vec<u8>>,Error> {
        match File::open(filename) {
            Ok(f) => {
                let mut read = 0;
                let mut reader = BufReader::new(f);
                let mut lines = Vec::new();
                loop {
                    let mut buf = Vec::new();
                    match reader.read_until(b'\n', &mut buf) {
                        Ok(0) => {
                            break;
                        },
                        Ok(num) => {
                            buf.pop();
                            lines.push(buf);
                            read += num;
                        },
                        _ => return Err(Error::CannotReadInputFile),
                    }
                }
                println!("{}", read);
                Ok(lines)
            },
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    eprintln!("{}: No such file or directory", filename);
                }
                Err(Error::CannotOpenInputFile)
            }
        }
    }

    fn try_edit(&mut self, filename: Option<String>) -> Result<(), Error> {
        match (self.dirty, self.warned) {
            (true, false) => {
                self.warned = true;
                Err(Error::WarningFileModified)
            },
            (false, true) => unreachable!(),
            _ => self.edit(filename),
        }
    }

    fn filename(&mut self, filename: Option<String>) -> Result<(), Error> {
        if filename.is_some() {
            self.default_filename = filename;
            Ok(())
        } else if let Some(filename) = self.default_filename.as_ref() {
            println!("{}", filename);
            Ok(())
        } else {
            Err(Error::NoCurrentFilename)
        }
    }

    fn mark_dirty(&mut self) {
        self.dirty = true;
        self.warned = false;
    }

    fn unmark_dirty(&mut self) {
        self.dirty = false;
        self.warned = false;
    }

    fn r#move(&mut self, addr: Address, dest_addr: Address) -> Result<(), Error> {
        self.buffer.r#move(addr, dest_addr)?;
        self.mark_dirty();
        Ok(())
    }

    fn transfer(&mut self, addr: Address, dest_addr: Address) -> Result<(), Error> {
        self.buffer.transfer(addr, dest_addr)?;
        self.mark_dirty();
        Ok(())
    }

    fn command(&mut self, command: String) -> Result<(),Error> {
        let (stdout, stderr) = self.command_inner(command)?;
        io::stdout().write_all(&stdout)
            .map_err(|_| Error::CannotWriteToStdout)?;
        io::stderr().write_all(&stderr)
            .map_err(|_| Error::CannotWriteToStderr)?;
        println!("!");
        Ok(())
    }

    fn command_inner(&mut self, command: String) 
        -> Result<(Vec<u8>,Vec<u8>),Error> {
        let command = if !command.is_empty() {
            let mut just_saw_backslash = false;
            let mut buf = String::new();
            for b in command.chars() {
                match b {
                    '%' => {
                        if !just_saw_backslash {
                            buf.push_str(self.default_filename.clone()
                                .ok_or(Error::NoCurrentFilename)?.as_str());
                        } else {
                            just_saw_backslash = false;
                            buf.push('%');
                        }
                    },
                    '\\' => {
                        if just_saw_backslash {
                            just_saw_backslash = false;
                            buf.push_str(r"\\");
                        } else {
                            just_saw_backslash = true;
                        }

                    },
                    b => {
                        if just_saw_backslash {
                            buf.push('\\');
                            just_saw_backslash = false;
                        }
                        buf.push(b);
                    },
                }
            }
            buf
        } else {
            command
        };

        let command = if command.starts_with("!") {
            if let Some(last) = self.last_command.take() {
                let s = last + &command[1..];
                println!("{}", s);
                self.last_command = Some(s.clone());
                Some(s)
            } else {
                None
            }
        } else {
            self.last_command = Some(command.clone());
            Some(command)
        };

        if command.is_none() {
            return Err(Error::NoPreviousCommand);
        }

        let out = std::process::Command::new("sh")
            .arg("-c")
            .arg(command.unwrap())
            .output()
            .map_err(|_| Error::CannotLaunchProcess)?;
        Ok((out.stdout, out.stderr))
    }
}

#[derive(PartialEq)]
enum AddressToken {
    BackwardSearch(Vec<u8>),
    Caret,
    Comma,
    Dollar,
    Dot,
    ForwardSearch(Vec<u8>),
    Mark(u8),
    Minus,
    Number(usize),
    Percent,
    Plus,
    Semicolon,
}

#[derive(Debug, PartialEq, Clone)]
enum Command {
    Edit(Option<String>),
    EditUnconditional(Option<String>),
    ExplainErrors,
    Change(Address),
    Delete(Address),
    Filename(Option<String>),
    Join(Address),
    Mark(Address, u8),
    Move(Address, Address),
    Number(Address),
    Prompt,
    Print(Address),
    PrintUnambiguous(Address),
    PrintOne(Address),
    PrintNumber(Address),
    Read(Address,Option<String>),
    ShellCommand(String),
    Substitute(Address, /* pattern: */ Vec<u8>, 
        /* replacement: */ Vec<ReplacementToken>, SubstitutionSuffix),
    Transfer(Address, Address),
    Append(Address),
    Insert(Address),
    Write(Address, Option<String>),
    WriteAppend(Address, Option<String>),
    WriteQuit(Address, Option<String>),
    Help,
    Quit,
    QuitUnconditional,
}

#[derive(Debug,Clone,PartialEq)]
enum ReplacementToken {
    Ampersand,
    Text(Vec<u8>),
    Backreference(usize),
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum SubstitutionSuffix {
    First,
    Global,
    Number(usize),
}

#[derive(Debug, Clone, PartialEq)]
enum Error {
    UnknownCommand,
    InvalidAddress,
    InvalidFilename,
    InvalidCommandSuffix,
    InvalidDestination,
    InvalidMarkCharacter,
    InvalidPatternDelimiter,
    NoCurrentFilename,
    NoPreviousCommand,
    NoPreviousPattern,
    CannotWriteFile,
    FilenameTooLong,
    LineTooLong,
    CannotReadStdin,
    WarningFileModified,
    CannotOpenOutputFile,
    CannotOpenInputFile,
    CannotReadInputFile,
    CannotFlushStdout,
    CannotWriteToStdout,
    CannotWriteToStderr,
    CannotGetTerminalSize,
    CannotLaunchProcess,
    UnexpectedAddress,
    TrailingBackslash,
    RegexError(String),
    NoMatch,
}

impl Display for Error {

    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", match self {
            Error::UnknownCommand => "unknown command",
            Error::InvalidAddress => "invalid address",
            Error::InvalidFilename => "invalid filename",
            Error::InvalidCommandSuffix => "invalid command suffix",
            Error::InvalidDestination => "invalid destination",
            Error::InvalidMarkCharacter => "invalid mark character",
            Error::InvalidPatternDelimiter => "invalid pattern delimiter",
            Error::NoCurrentFilename => "no current filename",
            Error::NoPreviousCommand => "no previous command",
            Error::NoPreviousPattern => "no previous pattern",
            Error::CannotWriteFile => "cannot write file",
            Error::FilenameTooLong => "filename too long",
            Error::LineTooLong => "line too long",
            Error::CannotReadStdin => "cannot read stdin",
            Error::WarningFileModified => "warning file modified",
            Error::CannotOpenOutputFile => "cannot open output file",
            Error::CannotOpenInputFile => "cannot open input file",
            Error::CannotReadInputFile => "cannot read input file",
            Error::CannotFlushStdout => "cannot flush stdout",
            Error::CannotWriteToStdout => "cannot write to stdout",
            Error::CannotWriteToStderr => "cannot write to stderr",
            Error::CannotGetTerminalSize => "cannot get terminal size",
            Error::CannotLaunchProcess => "cannot launch process",
            Error::UnexpectedAddress => "unexpected address",
            Error::TrailingBackslash => "trailing backslash",
            Error::RegexError(s) => &s[..],
            Error::NoMatch => "no match",
        })
    }
}

enum WriteMode {
    Create,
    Append,
}

fn main() {
    use Command::*;
    let mut cli = Cli::new();
    loop {
        let line = match cli.read_command() {
            Ok(s) => s,
            Err(e) => {
                eprintln!("?"); 
                cli.last_error = Some(e);
                if cli.explain_errors {
                    cli.help().expect("Does not fail");
                }
                continue;
            },
        };
        let res = match cli.parse_command(line) {
            Ok(cmd) => {
                match cmd {
                    Append(addr) => cli.append(addr),
                    Change(addr) => cli.change(addr),
                    Delete(addr) => cli.delete(addr),
                    Edit(filename) => cli.try_edit(filename),
                    EditUnconditional(filename) => cli.edit(filename),
                    ExplainErrors => cli.toggle_explain_errors(),
                    Filename(filename) => cli.filename(filename),
                    Help => cli.help(),
                    Insert(addr) => cli.insert(addr),
                    Join(addr) => cli.join(addr),
                    Mark(addr, c) => cli.mark(addr, c),
                    Move(addr, dest_addr) => cli.r#move(addr, dest_addr),
                    Number(addr) => cli.number(addr),
                    Print(addr) => cli.print(addr),
                    PrintNumber(addr) => cli.print_number(addr),
                    PrintOne(addr) => cli.print_one(addr),
                    PrintUnambiguous(addr) => cli.print_unambiguous(addr),
                    ShellCommand(command) => cli.command(command),
                    Substitute(_,_,_,_) => todo!(),
                    Read(addr, filename) => cli.read(addr, filename),
                    Prompt => cli.toggle_print_prompt(),
                    Transfer(addr, dest_addr) => cli.transfer(addr, dest_addr),
                    Quit => {
                        match cli.try_quit() {
                            Ok(()) => break,
                            e => e,
                        }
                    },
                    QuitUnconditional => break,
                    Write(addr, filename) => cli.write(addr, filename),
                    WriteAppend(addr, filename) => cli.write_append(addr, filename),
                    WriteQuit(addr, filename) => {
                        let _ = cli.write(addr, filename);
                        break;
                    },
                }
            },
            Err(e) => Err(e),
        };
        if let Err(e) = res {
            eprintln!("?"); 
            cli.last_error = Some(e);
            if cli.explain_errors {
                cli.help().expect("Does not fail");
            }
        }
    }
}
