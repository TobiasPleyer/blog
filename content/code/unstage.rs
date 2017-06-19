use std::fs::{self, DirEntry};
use std::path::Path;
use std::error::Error;
use std::io;
use std::time;
use std::fmt;


#[derive(Debug)]
enum UnstageError {
    IO(io::Error),
    SystemTime(time::SystemTimeError),
    BadPath,
    NoDirectory,
}

impl From<io::Error> for UnstageError {
    fn from(err: io::Error) -> UnstageError {
        UnstageError::IO(err)
    }
}

impl From<time::SystemTimeError> for UnstageError {
    fn from(err: time::SystemTimeError) -> UnstageError {
        UnstageError::SystemTime(err)
    }
}

impl fmt::Display for UnstageError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnstageError::IO(ref err) => err.fmt(f),
            UnstageError::SystemTime(ref err) => err.fmt(f),
            UnstageError::BadPath => write!(f, "Problems converting Path to String."),
            UnstageError::NoDirectory => write!(f, "Specified path is not a directory."),
        }
    }
}

impl Error for UnstageError {
    fn description(&self) -> &str {
        match *self {
            UnstageError::IO(ref err) => err.description(),
            UnstageError::SystemTime(ref err) => err.description(),
            UnstageError::BadPath => "Couldn't convert path",
            UnstageError::NoDirectory => "Not a directory",
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            UnstageError::IO(ref err) => Some(err),
            UnstageError::SystemTime(ref err) => Some(err),
            UnstageError::BadPath => None,
            UnstageError::NoDirectory => None,
        }
    }
}

#[derive(Debug)]
struct StageInfo {
    time: u64,
    path: String,
}

fn main() {
    let root = Path::new("test/test_folder");
    if root.is_dir() {
        for dir in root.read_dir().expect("failed to read directory") {
            match dir {
                Ok(dir) => {
                    let result = clear(&dir,3);
                    match result {
                        Ok(_) => {},
                        Err(e) => println!("{}", e.description()),
                    }
                },
                Err(e)  => { println!("Error: {}", e.description()); },
            }
        }
    } else {
        println!("The provided root path does not point to a directory!");
    }
}

fn clear(directory: &DirEntry, limit: usize) -> Result<(),UnstageError> {
    let mut v:Vec<StageInfo> = Vec::new();
    let path = directory.path();
    let path = path.as_path();
    if path.is_dir() {
        for dir in path.read_dir().expect("Unable to read sub directory") {
            let d = try!(dir);
            let si = try!(get_stage_info(&d));
            v.push(si);
        }
        v.sort_by_key(|ref x| {x.time});
        let mut i = v.iter().skip(limit);
        while let Some(si) = i.next() {
            let s = &si.path;
            let p = Path::new(s);
            try!(fs::remove_dir_all(&p));
        }
        Ok(())
    } else {
        Err(UnstageError::NoDirectory)
    }
}

fn get_stage_info(directory: &DirEntry) -> Result<StageInfo,UnstageError> {
    let meta = try!(directory.metadata());
    let modified = try!(meta.modified());
    let t = try!(modified.elapsed()).as_secs();
    let p = directory.path();
    if let Some(p) = p.to_str() {
        Ok(StageInfo {time: t, path: p.to_string()})
    } else {
        Err(UnstageError::BadPath)
    }
}
