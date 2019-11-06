#!/bin/sh -e

FESERVER_VERSION="V4.0.1"
# LOCATION_PATH="$HOME/feserver"
LOCATION_PATH="/usr/local/bin/feserver"
CONFIG_PATH="$HOME/feserver.yaml"
DOWNLOAD_1="https://s.vipkidstatic.com/beeschool/fx-fe/aa88ee8f6cd9e396f0410142fa4ea7c7"
DOWNLOAD_2="https://s.vipkidstatic.com/beeschool/fx-fe/5744fe0ae861f3b6afffa1dd4178a649.yaml"

echo "开始下载feserver ${FESERVER_VERSION}"
# Check whether the given command exists
has_cmd() {
  command -v "$1" > /dev/null 2>&1
}

# Check whether 'wget' command exists
has_wget() {
  has_cmd wget
}

# Check whether 'curl' command exists
has_curl() {
  has_cmd curl
}

has_file() {
  [ -e "$1" ]
}


# print a message to stderr and exit with error code
die() {
  echo "$@" >&2
  exit 1
}

# Download a URL to file using 'curl' or 'wget'.
dl_to_file() {
  if has_curl ; then
    if ! curl ${QUIET:+-sS} -L -o "$2" "$1"; then
      die "curl download failed: $1"
    fi
  elif has_wget ; then
    if ! wget ${QUIET:+-q} "-O$2" "$1"; then
      die "wget download failed: $1"
    fi
  else
    # should already have checked for this, otherwise this message will probably
    # not be displayed, since dl_to_stdout will be part of a pipeline
    die "Neither wget nor curl is available, please install one to continue."
  fi
}


diqye_main() {
  dl_to_file "$DOWNLOAD_1" "${LOCATION_PATH}"
  if has_file $CONFIG_PATH ; then
    echo "已经存在配置文件"
  else
    echo "没有检测到配置文件，为您下载一个"
    dl_to_file "$DOWNLOAD_2" "${CONFIG_PATH}"
  fi

  chmod 777 $LOCATION_PATH

  echo "安装成功 : $LOCATION_PATH "
  echo "配置文件 : $CONFIG_PATH (如没有自动创建模版)"
  echo "使用: sudo feserver"
}

diqye_main
