use std::ops::Index;
use crate::Error;
use crate::regex::Regex;

#[derive(Default,Clone)]
pub struct Buffer {
    inner: Vec<Vec<u8>>,
    cursor: Option<usize>,
    marks: [Option<usize>;26],
}

impl Buffer {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn mark(&mut self, (c, i): (u8, usize)) -> Result<(),Error> {
        if !c.is_ascii_lowercase() { return Err(Error::InvalidMarkCharacter); }
        if i > 0 && i <= self.len() {
            self.marks[(c - b'a') as usize] = Some(i);
            Ok(())
        } else {
            Err(Error::InvalidAddress)
        }
    }

    pub fn get_mark(&self, c: u8) -> Result<Option<usize>,Error> {
        if !c.is_ascii_lowercase() { return Err(Error::InvalidMarkCharacter); }
        Ok(self.marks[(c - b'a') as usize])
    }

    pub fn insert(&mut self, range: Address, lines: Vec<Vec<u8>>) -> Result<(), Error> {
        let index = match range {
            Address::Default => self.cursor.unwrap_or(1),
            Address::Line(x) | Address::Range(_, x) => {
                if x > 0 && x <= self.len() + 1 {
                    x
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
        };

        let mut tail = self.inner.split_off(index - 1);
        self.inner.extend_from_slice(&lines);
        self.inner.append(&mut tail);
        self.cursor = Some(index + lines.len() - 1);

        // If any mark is equal to or after the site of insertion, add lines.len() to it.
        // Else if a mark is before the site of insertion or has no value, no change.
        self.marks.iter_mut().filter(|m| match m {
            Some(x) if *x >= index => true,
            _ => false,
        }).for_each(|m| { *m = Some(m.take().unwrap() + lines.len()); });

        Ok(())
    }

    pub fn append(&mut self, range: Address, lines: Vec<Vec<u8>>) -> Result<(), Error> {
        let index = match range {
            Address::Default => self.cursor.unwrap_or(0),
            Address::Line(x) | Address::Range(_, x) => {
                if x <= self.len() {
                    x
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
        };
        let mut tail = self.inner.split_off(index);
        self.inner.extend_from_slice(&lines);
        self.inner.append(&mut tail);
        self.cursor = Some(index + lines.len());

        // If any mark is strictly after the site of insertion, add lines.len() to it.
        // Else if a mark is at or before the site of insertion or has no value, no change.
        self.marks.iter_mut().filter(|m| match m {
            Some(x) if *x > index => true,
            _ => false,
        }).for_each(|m| *m = Some(m.take().unwrap() + lines.len()));
        Ok(())
    }

    pub fn delete(&mut self, address: Address) -> Result<(), Error> {
        let (idx, range_start, range_end) = match address {
            Address::Default => {
                if let Some(cur) = self.cursor {
                    self.inner.remove(cur - 1);
                    (cur, cur, cur)
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
            Address::Line(num) => {
                if num > 0 && num <= self.len() {
                    self.inner.remove(num - 1);
                    (num, num, num)
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
            Address::Range(x, y) => {
                if x <= y && x > 0 && y <= self.len() {
                    self.inner.drain(x-1..=y-1);
                    (x, x, y)
                } else {
                    return Err(Error::InvalidAddress);
                }

            },
        };
        self.cursor = if idx <= self.len() {
            Some(idx)
        } else if self.len() > 0 {
            Some(self.len())
        } else {
            None
        };

        // If a mark is after the deleted range, subtract the length of the range.
        // Else if a mark is contained in the deleted range, invalidate the mark.
        self.marks.iter_mut().filter(|m| m.is_some()).for_each(|m| {
            *m = match m.take().unwrap() {
                x if x < range_start => Some(x),
                x if x <= range_end => None,
                x => Some(x - (range_end - range_start + 1)),
            };
        });

        Ok(())
    }

    pub fn change(&mut self, address: Address, lines: Vec<Vec<u8>>)
        -> Result<(), Error> {
        let (idx, range_start, range_end) = match address {
            Address::Default => {
                if let Some(cur) = self.cursor {
                    self.inner.remove(cur - 1);
                    (cur - 1, cur, cur)
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
            Address::Line(num) => {
                if num > 0 && num <= self.len() {
                    self.inner.remove(num - 1);
                    (num - 1, num, num)
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
            Address::Range(x, y) => {
                if x <= y && x > 0 && y <= self.len() {
                    self.inner.drain(x-1..=y-1);
                    (x - 1, x, y)
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
        };

        // Invalidate marks in the range range_start..=range_end
        // If a mark is before range_start, no change.
        // If a mark is after range_end, add the net change in number of lines before.
        self.marks.iter_mut().filter(|m| m.is_some())
            .for_each(|m| *m = match m.take().unwrap() {
                x if x < range_start => Some(x),
                x if x >= range_start && x <= range_end => None,
                x => Some(x + lines.len() - (range_end - range_start + 1)),
        });

        let mut tail = self.inner.split_off(idx);
        self.inner.extend_from_slice(&lines);
        self.inner.append(&mut tail);
        self.cursor = if !self.inner.is_empty() {
            Some(idx + lines.len())
        } else {
            None
        };

        Ok(())
    }

    pub fn fetch(&mut self, address: Address) -> Result<Vec<&[u8]>,Error> {
        match address {
            Address::Default => {
                if let Some(cur) = self.cursor {
                    Ok(vec![&self[cur]])
                } else {
                    Err(Error::InvalidAddress)
                }
            },
            Address::Line(x) => {
                if x > 0 && x <= self.len() {
                    Ok(vec![&self[x]])
                } else {
                    Err(Error::InvalidAddress)
                }
            },
            Address::Range(x, y) => {
                if x <= y && x > 0 && y <= self.len() {
                    Ok((x..=y).map(|i| &self[i][..]).collect())
                } else {
                    Err(Error::InvalidAddress)
                }
            },
        }
    }

    pub fn fetch_change_address(&mut self, address: Address) 
        -> Result<Vec<&[u8]>,Error> {
        match address {
            Address::Default => {
                if let Some(cur) = self.cursor {
                    Ok(vec![&self[cur]])
                } else {
                    Err(Error::InvalidAddress)
                }
            },
            Address::Line(x) => {
                if x > 0 && x <= self.len() {
                    self.cursor = Some(x);
                    Ok(vec![&self[x]])
                } else {
                    Err(Error::InvalidAddress)
                }
            },
            Address::Range(x, y) => {
                if x <= y && x > 0 && y <= self.len() {
                    self.cursor = Some(y);
                    Ok((x..=y).map(|i| &self[i][..]).collect())
                } else {
                    Err(Error::InvalidAddress)
                }
            },
        }
    }

    pub fn plus(&self, n: usize) -> Result<usize, Error> {
        if let Some(cur) = self.cursor {
            if cur + n <= self.len() {
                return Ok(cur + n);
            }
        }
        Err(Error::InvalidAddress)
    }

    pub fn minus(&self, n: usize) -> Result<usize, Error> {
        if let Some(cur) = self.cursor {
            if cur > n {
                return Ok(cur - n);
            }
        }
        Err(Error::InvalidAddress)
    }

    pub fn end(&self) -> Result<usize, Error> {
        if self.inner.is_empty() {
            Err(Error::InvalidAddress)
        } else {
            Ok(self.inner.len())
        }
    }

    pub fn cur(&self) -> Result<usize, Error> {
        self.cursor.ok_or(Error::InvalidAddress)
    }

    pub fn all(&self) -> Result<Address, Error> {
        if self.inner.is_empty() {
            Err(Error::InvalidAddress)
        } else {
            Ok(Address::Range(1, self.len()))
        }
    }

    pub fn is_valid(&self, addr: Address) -> bool {
        match addr {
            Address::Default => !self.cursor.is_none(),
            Address::Line(num) => num > 0 && num <= self.len(),
            Address::Range(x, y) => x <= y && x > 0 && y <= self.len(),
        }
    }

    pub fn load(&mut self, buf: Vec<Vec<u8>>) {
        self.inner = buf;
        self.cursor = if self.inner.is_empty() {
            None
        } else {
            Some(self.inner.len())
        };
        
        // Invalidate all marks
        self.marks.iter_mut().for_each(|m| *m = None);
    }

    // Copy lines from source addres to after destination address.
    pub fn transfer(&mut self, src: Address, dst: Address) -> Result<(), Error> {
        let dst = match dst {
            Address::Default => self.cursor.ok_or(Error::InvalidAddress)?,
            Address::Line(x) | Address::Range(_, x) => if x <= self.len() { 
                x 
            } else {
                return Err(Error::InvalidAddress);
            },
        };

        let lines = match src {
            Address::Default => {
                vec![self[self.cursor.ok_or(Error::InvalidAddress)?].clone()]
            },
            Address::Line(num) => if num > 0 && num <= self.len() {
                vec![self[num].clone()]
            } else {
                return Err(Error::InvalidAddress);
            },
            Address::Range(x, y) => {
                if x <= y && x > 0 && y <= self.len() {
                    (x..=y).map(|i| self[i].clone()).collect()
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
        };

        // If a mark is <= dst, no change. If a mark is > dst, + lines.len()
        self.marks.iter_mut().filter(|m| m.is_some()).for_each(|m| {
            *m = match m.take().unwrap() {
                x if x <= dst => Some(x),
                x => Some(x + lines.len()),
            }
        });

        self.cursor = Some(dst + lines.len());
        let tail = self.inner.split_off(dst);
        self.inner.extend(lines);
        self.inner.extend(tail);

        Ok(())
    }

    // Move lines from source addres to after destination address.
    pub fn r#move(&mut self, src: Address, dst: Address) -> Result<(), Error> {

        let mut dst = match dst {
            Address::Default => self.cursor.ok_or(Error::InvalidAddress)?,
            Address::Line(x) | Address::Range(_, x) => if x <= self.len() { 
                x 
            } else {
                return Err(Error::InvalidAddress);
            },
        };

        let old_dst = dst;

        let (range_start, range_end, lines) = match src {
            Address::Default => {
                let x = self.cursor.ok_or(Error::InvalidAddress)?;
                if dst == x { 
                    self.cursor = Some(dst);
                    return Ok(());
                } else if x < dst {
                    dst -= 1;
                }
                (x, x, vec![self.inner.remove(x-1)])
            }
            Address::Line(x) => {
                if x > 0 && x <= self.len() {
                    if x == dst {
                        self.cursor = Some(dst);
                        return Ok(());
                    } else if x < dst {
                        dst -= 1;
                    }
                    (x, x, vec![self.inner.remove(x-1)])
                } else {
                    return Err(Error::InvalidAddress);
                }
            },
            Address::Range(x, y) => {
                if y >= x && x > 0 && y <= self.len() { 
                    if dst >= x && dst < y {
                        return Err(Error::InvalidDestination);
                    } else if dst == y {
                        self.cursor = Some(dst);
                        return Ok(());
                    } else if dst > y {
                        dst -= y - x + 1;
                    }
                    let mut tail = self.inner.split_off(x-1);
                    self.inner.extend(tail.split_off(y - x + 1));
                    (x, y, tail)

                } else {
                    return Err(Error::InvalidAddress);
                }
            },
        };

        self.cursor = Some(dst + lines.len());
        let tail = self.inner.split_off(dst);
        self.inner.extend(lines);
        self.inner.extend(tail);

        // If in range_start..=range_end, dst + x - range_start + 1
        // Else if before dst, no change
        // Else if after dst and before range_start, + (range_end - range_start + 1)
        // Else if > dst and > range_end, no change
        // Else if > range_end and <= dst, -(range_end - range_start + 1)
        let range_len = range_end - range_start + 1;
        self.marks.iter_mut().filter(|m| m.is_some()).for_each(|m| {
            *m = match m.take().unwrap() {
                x if x <= old_dst && x < range_start => Some(x),
                x if x <= old_dst && x > range_end => Some(x - range_len),
                x if x <= old_dst => Some(dst + x - range_start + 1),
                x if x < range_start => Some(x + range_len),
                x if x > range_end => Some(x),
                x => Some(dst + x - range_start + 1),
            };
        });

        Ok(())
    }

    // (.,.+1)j
    pub fn join(&mut self, addr: Address) -> Result<(), Error> {
        let (range_start, range_end) = match addr {
            Address::Default => {
                if self.len() < 2 { return Err(Error::InvalidAddress); }
                let cur = self.cursor.unwrap();
                if cur + 1 > self.len() { return Err(Error::InvalidAddress); }
                (cur, cur + 1)
            },
            Address::Line(_) => {
                return Ok(());
            },
            Address::Range(x, y) => {
                if x > y || x == 0 || y > self.len() {
                    return Err(Error::InvalidAddress);
                } else if x == y {
                    return Ok(());
                }
                (x, y)
            },
        };

        let mut lines = self.inner.drain(range_start-1..=range_end-1)
            .collect::<Vec<_>>();
        let line = lines.into_iter().fold(vec![], |mut acc,e| {
            acc.extend(&e);
            acc
        });
        self.inner.insert(range_start-1, line);
        self.cursor = Some(range_start);
        self.marks.iter_mut().filter(|m| m.is_some()).for_each(|m| {
            *m = match m.take().unwrap() {
                x if x >= range_start && x <= range_end => None,
                x if x < range_start => Some(x),
                x => Some(x - (range_end - range_start)),
            }
        });
        Ok(())
    }

    // Translate forward search to line number
    pub fn forward_search(&self, pattern: &[u8]) -> Result<usize,Error> {
        if let Some(cur) = self.cursor {
            let mut re = Regex::new(pattern)?;
            let mut p = cur % self.len() + 1;
            while p != cur {
                if re.r#match(&self[p]).is_ok() {
                    return Ok(p);
                }
                p = p % self.len() + 1;
            }
            if re.r#match(&self[cur]).is_ok() {
                return Ok(cur);
            }
        }
        Err(Error::NoMatch)
    }
    
    pub fn backward_search(&self, pattern: &[u8]) -> Result<usize,Error> {
        if let Some(cur) = self.cursor {
            let mut re = Regex::new(pattern)?;
            let mut p = if cur - 1 == 0 { self.len() } else { cur - 1 };
            while p != cur {
                if re.r#match(&self[p]).is_ok() {
                    return Ok(p);
                }
                p = if p - 1 == 0 { self.len() } else { p - 1 };
            }
        }
        Err(Error::NoMatch)
    }
}

impl Index<usize> for Buffer {
    type Output = Vec<u8>;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.inner[idx - 1]
    }
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum Address {
    Default,
    Line(usize),
    Range(usize, usize),
}

enum Undo {
    Move(Address, Address),
    Transfer(Address),
    Change(Vec<Vec<u8>>, Address),
    Delete(Vec<Vec<u8>>, Address),
    Append(Address),
    Insert(Address),
    Join(Vec<usize>, Address),
    Read(),
}
