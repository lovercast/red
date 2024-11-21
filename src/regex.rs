
use std::ffi::CString;
use std::ptr::{from_ref,from_mut};

use libc::*;

use crate::Error;

pub struct Regex {
    inner: regex_t,
    pattern: CString,
}

const NMATCH: usize = 10;

#[derive(Default,Debug)]
pub struct Match {
    inner: [Option<(usize,usize)>;NMATCH],
}

impl Regex {
    pub fn new(pattern: &[u8]) -> Result<Self,Error> {
        let mut buf = Self {
            inner: unsafe { std::mem::uninitialized() },
            pattern: CString::new(pattern)
                .expect("No internal NULL bytes"),
        };

        let res = unsafe {
            regcomp(from_mut(&mut buf.inner), 
                buf.pattern.as_ptr(), 
                REG_BASIC) 
        };

        match res {
            0 => Ok(buf),
            error => Err(buf.error(error)),
        }
    }

    pub fn r#match(&self, haystack: &[u8]) -> Result<Match,Error> {
        let haystack = CString::new(haystack)
            .expect("No internal NULL bytes");

        let mut buf: [regmatch_t;NMATCH] = unsafe {
            std::mem::uninitialized()
        };

        let res = unsafe {
            regexec(
                from_ref(&self.inner),
                haystack.as_ptr(),
                buf.len(),
                buf.as_mut_ptr(),
                0,
            )
        };

        match res {
            0 => {
                let mut r#match = Match::default();
                for (i,ro) in buf.into_iter().enumerate() {
                    if ro.rm_so >= 0 {
                        r#match.inner[i] = Some((ro.rm_so as usize,
                                ro.rm_eo as usize));
                    }
                }
                Ok(r#match)

            },
            error => Err(self.error(error)),
        }
    }

    fn error(&self, error: c_int) -> Error {
        let mut errbuf = vec![0_u8;128];
        let errbuf_size = errbuf.len();

        let size = unsafe { 
            regerror(error, 
                from_ref(&self.inner),
                errbuf.as_mut_ptr() as *mut c_char,
                errbuf_size) 
        };

        // Fulfill invariants for creating a CString
        errbuf.truncate(size);

        let cstring = unsafe {
            CString::from_vec_with_nul_unchecked(errbuf)
        };

        Error::RegexError(cstring.into_string()
            .expect("regerror outputs a valid C-String"))
    }
}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe { regfree(from_mut(&mut self.inner)); }
    }
}

#[cfg(test)]
mod test {
    use crate::regex::*;
    #[test]
    fn foo() {
        let r = Regex::new(br"\<.").expect("Good Regex");
        let s = r"hello ma baby hello ma honey";
        let mut slice = s.as_bytes();

        println!("outside loop");
        while let Ok(r#match) = r.r#match(slice) {
            dbg!(&r#match);
            slice = match r#match.inner[0] {
                Some((_,x)) => &slice[x..],
                _ => unreachable!(),
            };
        }
    }
}
