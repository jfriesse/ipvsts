#!/bin/bash

#
# Copyright (c) 2011, Red Hat, Inc.
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND RED HAT, INC. DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL RED HAT, INC. BE LIABLE
# FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
# OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
# CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#

#
# Author: Jan Friesse <jfriesse@redhat.com>
#

#
# Script for make release
#

cd_to_proj_root() {
    while [ ! -d ".git" ] && [ "$PWD" != "/" ];do
        cd ..
    done

    [ "$PWD" != "/" ]
    return $?
}

failure() {
    echo "Cannot find project .git directory"
    exit 1
}

cd_to_proj_root || failure

[ "$1" != "" ] && VER=$1 || VER=`date "+%Y%m%d"`

git tag -a "$VER" -m "Tag version $VER" || exit 1
git archive --format=tar --prefix="ipvsts-$VER/" "$VER" | gzip -9 > ipvsts-$VER.tar.gz || exit 1

echo "Archive created."
echo "Next steps:"
echo "scp $PWD/ipvsts-$VER.tar.gz fedorahosted.org:ipvsts"
echo "git push --tags"
