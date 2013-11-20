#!/bin/sh -e

github_download() {
	local repo=$1
	local ver=$2
	local tgt=$3
	if [ ! -d "$tgt" ]; then
		(
		mkdir -p "$tgt"
		cd "$tgt"
		if which wget > /dev/null 2>&1; then
			wget https://github.com/${repo}/archive/${ver}.tar.gz -O - | \
				tar --strip-components=1 -xvzf -
		fi
		)
	fi
}

v8_repo=v8/v8
v8_ver=3.23.6
v8_dir="$(pwd)/.deps/v8"

gyp_repo=svn2github/gyp
gyp_ver=60947b8014d56749a3def7404c827a57e64a264b   # revision 1685
gyp_dir="$v8_dir/build/gyp"

github_download "$v8_repo" "$v8_ver" "$v8_dir"
github_download "$gyp_repo" "$gyp_ver" "$gyp_dir"

cd "$v8_dir"
make i18nsupport=off native
cd ..
mkdir -p lib

if [ ! -d "include" ]; then
	cp -a "$v8_dir/include" ./
fi

ar rcsT ./lib/libv8_static.a \
	"$v8_dir/out/native/obj.target/tools/gyp/libv8_base"*.a \
	"$v8_dir/out/native/obj.target/tools/gyp/libv8_snapshot.a"
