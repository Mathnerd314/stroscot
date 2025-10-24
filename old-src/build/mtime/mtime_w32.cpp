#ifdef _WIN32
#include <sstream>
#include <windows.h>

string DirName(const string& path) {
  static const char kPathSeparators[] = "\\/";
  static const char* const kEnd = kPathSeparators + sizeof(kPathSeparators) - 1;

  string::size_type slash_pos = path.find_last_of(kPathSeparators);
  if (slash_pos == string::npos)
    return string();  // Nothing to do.
  while (slash_pos > 0 &&
         std::find(kPathSeparators, kEnd, path[slash_pos - 1]) != kEnd)
    --slash_pos;
  return path.substr(0, slash_pos);
}

TimeStamp Stat(const string& path, string* err) const {
  // MSDN: "Naming Files, Paths, and Namespaces"
  // http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx
  if (!path.empty() && path[0] != '\\' && path.size() > MAX_PATH) {
    ostringstream err_stream;
    err_stream << "Stat(" << path << "): Filename longer than " << MAX_PATH
               << " characters";
    *err = err_stream.str();
    return -1;
  }

  string dir = DirName(path);
  string base(path.substr(dir.size() ? dir.size() + 1 : 0));
  if (base == "..") {
    // StatAllFilesInDir does not report any information for base = "..".
    base = ".";
    dir = path;
  }

  transform(dir.begin(), dir.end(), dir.begin(), ::tolower);
  transform(base.begin(), base.end(), base.begin(), ::tolower);

  dir = dir.empty() ? "." : dir;
  stamps = DirCache();

  if (files_in_dir < 3) {
    WIN32_FILE_ATTRIBUTE_DATA attrs;
    if (!GetFileAttributesExA(path.c_str(), GetFileExInfoStandard, &attrs)) {
      DWORD win_err = GetLastError();
      if (win_err == ERROR_FILE_NOT_FOUND || win_err == ERROR_PATH_NOT_FOUND)
        return 0;
      *err = "GetFileAttributesEx(" + path + "): " + GetLastErrorString();
      return -1;
    }
    return TimeStampFromFileTime(attrs.ftLastWriteTime);
  } else {
    // Getting the timestamps for the whole directory is 30% faster
    WIN32_FIND_DATAA ffd;
    HANDLE find_handle = FindFirstFileExA((dir + "\\*").c_str(), FindExInfoBasic, &ffd, FindExSearchNameMatch, NULL, 0);

    if (find_handle == INVALID_HANDLE_VALUE) {
      DWORD win_err = GetLastError();
      if (win_err == ERROR_FILE_NOT_FOUND || win_err == ERROR_PATH_NOT_FOUND)
        return true;
      *err = "FindFirstFileExA(" + dir + "): " + GetLastErrorString();
      return false;
    }
    do {
      if (wanted_names.find(ffd.cFileName)) {
        stamps->insert(ffd.cFileName, TimeStampFromFileTime(ffd.ftLastWriteTime)));
      }
    } while (FindNextFileA(find_handle, &ffd));
    FindClose(find_handle);
    return 0;
  }
}

#endif /*_WIN32*/
