#lang notes

@block{@block-name{rsync}
  # rsync uses ssh to facilitate file transfers. It doesn't do a bi-directional
  # syncs; it's an utility for copying

  # rsync must be installed on the target machine! Test if it is installed:
  # -v (or -s or --search)
  command --search rsync

  # March 2022: the scp Secure Copy Protocol is obsolete! Use sftp or rsync

  # --copy-links, -L         transform symlink into referent file/dir
  rsync -Lav bost@"@"lukas:/run/current-system/configuration.scm $dotf/

  # recursive copy `dotfiles` and `cheat` to server:~/dev/
  # i.e. create `server:~/dev/dotfiles/` and `server:~/dev/cheat/`
  rsync -avz dotfiles cheat server:~/dev/
  # recursive copy of only the content of `dotfiles` and `cheat`.
  # i.e. create only the `server:~/dev/`
  rsync -avz dotfiles/ cheat/ server:~/dev

  # recursive copy only certain types of files using include option
  rsync -havz --include="*/" --include="*.sh" --exclude="*" "$src" "$dst"

  # copy multiple files from remote machine to a local machine
  rsync -a USER@"@"HOST:/remote/path/file1 :/remote/path/file2 /local/path

  # --archive - transfer / copy also the file metadata
  # copy files from 'src' to 'dst' excluding everything in 'dir' directories
  # --dry-run -n --human-readable -h --archive -a --verbose -v --compress -z
  rsync -nhavz          --exclude='dir' --exclude='*.jpg' src/ dst
  rsync -nhavz --delete --exclude='dir' --exclude='*.jpg' src/ dst | rg deleting
  # copy files from 'src' to 'dst' excluding everything in 'dir' directories
  # exclude hidden files and directories
  rsync -nhav          --exclude=".*" --exclude=".*/" src/ dst
  rsync -nhav --delete --exclude=".*" --exclude=".*/" src/ dst | grep deleting

  # move content of a directory within another directory with the same folders
  rsync -nha          --remove-source-files backup/ backupArchives
  rsync -nha --delete --remove-source-files backup/ backupArchives | rg deleting

  # rsync options: short / long versions
  # -h --human-readable
  # -a --archive # -rlptgoD (no -H,-A,-X); recursive, preserve almost everything
  # -v --verbose
  # -z --compress
  # -r --recursive
  # -n --dry-run
  # -p --perms   # preserve permissions

  # show / monitor overall progress without seeing progress for each individual
  # file with overall percentage and estimated time remaining for the entire
  # operation
  rsync --archive --info=progress2 /src/path/to/dir /dst/path/

  # bash: transfer with timeout
  SRC_DIR="/path/to/source/"
  DST_DIR="/path/to/destination/"
  find "$SRC_DIR" -type f | while read -r FILE; do
      # Use timeout to limit the transfer time for each file
      timeout 1m rsync -avh --remove-source-files "$FILE" "$DST_DIR"
      if [ $? -ne 0 ]; then
          echo "Transfer of $FILE failed or timed out. Skipping to next file."
      fi
  done

}
