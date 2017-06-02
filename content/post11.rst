Different Angles
################

:date: 2017-06-03
:tags: rust, haskell, python, c
:category: Programming
:authors: Tobias Pleyer
:summary: Tackeling the same programming task with different languages

Many roads lead to Rome
=======================

If you happen to know more than one programming language it can be a fun challange to
solve the same programming task with more than one programming language. It can be both,
a mental challenge and a chance to look differently at the same thing.

The Task
========

We have the following situation: A folder with subfolders with subfolders. What we want
is to iterate over all the subfolders of every subfolder of the original folder and
only keep the *N* newest, the others shall be deleted.

In my concrete real life situation the subfolders were Git branches and the subsubfolders were
the commits of these branches.

The contestents will be: Python, C, Rust and Haskell

**Disclaimer:** I do not have equal skill levels for each of these languages, so it can be
that my solutions are far from ideal in some cases. But that is not part of the challenge.
We won't look at speed, performance, memory footprint or alike. The only criterion is that
the implementation does the job. Error handling is optional.

Solutions
=========

Python
------

.. code-block:: python

    # yet to follow

C-Code
------

.. code-block:: c

    #include <sys/types.h>
    #include <sys/stat.h>
    #include <unistd.h>
    #include <dirent.h>
    #include <stdlib.h>
    #include <stdio.h>
    #include <string.h>
    #include <ftw.h>

    enum {
        RET_OK=0,
        RET_BAD_ARGUMENT,
        RET_OPEN_ERR,
        RET_NOT_FOUND,
        RET_NO_PERMISSION,
        RET_MALLOC_ERROR,
        RET_LSTAT_ERROR
    };

    #define PATH_BUFFER 1024
    #define ARRAY_SIZE 128
    #define KEEP_LIMIT 3

    static char* curr_path;

    typedef struct entry {
        long int timestamp;
        char dir_name[PATH_BUFFER];
    } entry_t;

    static entry_t commits[ARRAY_SIZE];

    static int entry_comp(const void*, const void*);
    static int lookat(char *);
    static int clean(char*, unsigned short);
    static int append(char* path, size_t pos, char* add);
    typedef struct FTW sFTW;
    static int ftw_deleter(const char* fpath, const struct stat* sb, int typeflag, sFTW *ftwbuf);
    static int delete_dir(const char* path);

    extern int nftw(const char* dirpath, int(*fn) (const char* fpath, const struct stat* sb, int typeflag, sFTW* ftwbuf), int nopenfd, int flags);

    int main(int argc, char* argv[])
    {
        int ret;

        if (argc < 2) {
            printf("Usage: %s dir_name\n", argv[0]);
            exit(RET_BAD_ARGUMENT);
        }

        curr_path = malloc(PATH_BUFFER);
        if (curr_path != NULL) {
            strncpy(curr_path, argv[1], PATH_BUFFER);
            ret = lookat(curr_path);
        }
        else {
            ret = RET_MALLOC_ERROR;
        }
        return ret;
    }

    static int entry_comp(const void *elem1, const void *elem2) {
        /*
         * Custom sorting function
         * Sort the entries by their modification timestamp
         * The bigger the timestamp, the newer the commit.
         * Sort by descending order.
         */
        unsigned int t1 = ((entry_t*)elem1)->timestamp;
        unsigned int t2 = ((entry_t*)elem2)->timestamp;
        if (t1 > t2) return -1;
        if (t1 < t2) return 1;
        return 0;
    }

    static int ftw_deleter(const char* fpath, const struct stat* sb, int typeflag, sFTW* ftwbuf) {
        int ret = 0;
        if (typeflag == FTW_F) {
            ret = remove(fpath);
        }
        return ret;
    }

    static int delete_dir(const char* path) {
        int ret;
        ret = nftw(path, ftw_deleter, 25, 0);
        if ( ret == 0 ) {
            ret = rmdir(path);
        }
        return ret;
    }

    static int lookat(char *path) {
        int ret = RET_OK;
        int r;
        DIR* root;
        struct dirent* dirp;
        size_t path_len;

        root = opendir(path);
        if (root == NULL) {
            ret = RET_OPEN_ERR;
            goto end;
        }
        path_len = strlen(curr_path);
        while ((dirp = readdir(root)) != NULL) {
            if ( strcmp(dirp->d_name, ".") == 0 || strcmp(dirp->d_name, "..") == 0 ) {
                continue;
            }
    #ifdef DEBUG
            printf("\nFound branch %s\n", dirp->d_name);
    #endif
            append(curr_path, path_len, dirp->d_name);
            r = clean(curr_path, KEEP_LIMIT);
            if (r != RET_OK) {
                ret = r;
                goto end;
            }
        }
    end:
        closedir(root);
        return ret;
    }

    static int clean(char* dirname, unsigned short limit) {
        int ret = RET_OK;
        DIR* dp;
        struct dirent* dirp;
        struct stat statbuf;
        entry_t* commit;
        size_t commit_idx;
        size_t path_len;

        dp = opendir(dirname);
        if (dp == NULL) {
            ret = RET_OPEN_ERR;
            goto end;
        }
        path_len = strlen(curr_path);
        commit_idx = 0;
        while ((dirp = readdir(dp)) != NULL) {
            if ( strcmp(dirp->d_name, ".") == 0 || strcmp(dirp->d_name, "..") == 0 ) {
                continue;
            }
            append(curr_path, path_len, dirp->d_name);
            if (lstat(curr_path, &statbuf) < 0) {
                ret = RET_LSTAT_ERROR;
                goto end;
            }
            commit = &commits[commit_idx++];
            strncpy(commit->dir_name, curr_path, PATH_BUFFER);
            commit->timestamp = statbuf.st_mtim.tv_sec;
    #ifdef DEBUG
            printf("\tFound commit %s with mtime %ld\n", dirp->d_name, statbuf.st_mtim.tv_sec);
    #endif
        }
        qsort(commits, commit_idx, sizeof(entry_t), entry_comp);
        int i;
    #ifdef DEBUG
        printf("After sorting\n");
        for(i=0; i<commit_idx; i++) {
            printf("\tCommit %s with mtime %ld\n", commits[i].dir_name, commits[i].timestamp);
        }
    #endif
        if (commit_idx > KEEP_LIMIT) {
            for (i=KEEP_LIMIT; i<commit_idx; i++) {
    #ifdef DEBUG
                printf("Deleting %s\n", commits[i].dir_name);
    #endif
                delete_dir(commits[i].dir_name);
            }
        }
    end:
        closedir(dp);
        return ret;
    }

    static int append(char* path, size_t pos, char* add) {
        path[pos++] = '/';
        path[pos] = 0;
        strncpy(&path[pos], add, PATH_BUFFER-pos);
        return 0;
    }

Rust
----

.. code-block:: rust

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
        let root = Path::new("/home/tobias/Programming/unstage/test/test_folder");
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

Haskell
-------

.. code-block:: haskell

    module Main where

    import Control.Monad
    import System.Directory
    import System.Environment
    import System.FilePath
    import Data.Time.Clock
    import Data.List

    main = do
      args <- getArgs
      case args of
        [] -> putStrLn "Usage: unstage directory"
        (d:xs) -> do
          putStrLn d
          dirs <- getDirectoryContents d
          let abs_dirs = map (d </>) (filter noDot dirs)
          forM_ abs_dirs (sortAndDelete 3)


    sortAndDelete limit branch = do
      putStrLn branch
      commit_infos <- getInfo branch
      let delete_candidates = drop limit (sortBy sortFunc commit_infos)
      forM_ delete_candidates (removeDirectoryRecursive . fst)
      where
        sortFunc a b = compare (snd b) (snd a)


    getInfo branch = do
      commits <- getDirectoryContents branch
      let abs_commits = map (branch </>) (filter noDot commits)
      mod_times <- mapM getModificationTime abs_commits
      return (zip abs_commits mod_times)


    noDot :: FilePath -> Bool
    noDot "." = False
    noDot ".." = False
    noDot _ = True

Conclusion
==========
