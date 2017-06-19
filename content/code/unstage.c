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
