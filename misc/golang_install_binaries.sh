#!/usr/bin/env bash

which go > /dev/null
if [ "$?" != "0" ]; then
    echo "Not found command: go"
    exit -1
fi

if [ -z $GOPATH ]; then
    echo "GOPATH not set"
    exit -1
fi

echo "Will install binaries..."

env GO111MODULE="auto" go install golang.org/x/tools/gopls@latest
env GO111MODULE="auto" go install golang.org/x/tools/cmd/goimports@latest
env GO111MODULE="auto" go install golang.org/x/tools/cmd/gorename@latest
env GO111MODULE="auto" go install golang.org/x/tools/cmd/guru@latest
env GO111MODULE="auto" go install golang.org/x/lint/golint@latest
env GO111MODULE="auto" go install github.com/go-delve/delve/cmd/dlv@latest

# go get -u -v github.com/klauspost/asmfmt/cmd/asmfmt
# go get -u -v github.com/kisielk/errcheck
# go get -u -v github.com/davidrjenni/reftools/cmd/fillstruct
# go get -u -v github.com/rogpeppe/godef
# go get -u -v github.com/zmb3/gogetdoc
# go get -u -v github.com/golangci/golangci-lint/cmd/golangci-lint
# go get -u -v github.com/fatih/gomodifytags
# go get -u -v github.com/jstemmer/gotags
# go get -u -v github.com/josharian/impl
# go get -u -v honnef.co/go/tools/cmd/keyify
# go get -u -v github.com/fatih/motion
# go get -u -v github.com/koron/iferr
