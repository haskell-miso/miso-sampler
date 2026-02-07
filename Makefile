.PHONY= update build optim

all: update build optim

js: update-js build-js

update:
	wasm32-wasi-cabal update

repl: update
	wasm32-wasi-cabal repl app -finteractive --repl-options='-fghci-browser -fghci-browser-port=8080'

watch:
	ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch . --debounce 50ms --command 'wasm32-wasi-cabal repl app -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"'

build:
	wasm32-wasi-cabal build 
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin app | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

ghcup-update:
	cabal --allow-newer=base,template-haskell --with-compiler=wasm32-wasi-ghc-9.14 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.14 --with-hsc2hs=wasm32-wasi-hsc2hs-9.14 --with-haddock=wasm32-wasi-haddock-9.14 update

ghcup-build:
	echo ==============================
	echo I wanted to run this:
	echo ==============================
	echo cabal --allow-newer=base,template-haskell --with-compiler=wasm32-wasi-ghc-9.14 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.14 --with-hsc2hs=wasm32-wasi-hsc2hs-9.14 --with-haddock=wasm32-wasi-haddock-9.14 build 
	echo but do I have these?
	echo ==============================
	echo -n 'wasm32-wasi-ghc-9.14 '
	which wasm32-wasi-ghc-9.14 >/dev/null 2>&1 && echo yes || echo no
	echo -n 'wasm32-wasi-ghc-pkg-9.14 '
	which wasm32-wasi-ghc-pkg-9.14 >/dev/null 2>&1 && echo yes || echo no
	echo -n 'wasm32-wasi-hsc2hs-9.14 '
	which wasm32-wasi-hsc2hs-9.14 >/dev/null 2>&1 && echo yes || echo no
	echo -n 'wasm32-wasi-haddock-9.14 '
	which wasm32-wasi-haddock-9.14 >/dev/null 2>&1 && echo yes || echo no
	echo ==============================

install-wasm-via-ghcup:
	curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | SKIP_GHC=1 sh
	source ~/.ghc-wasm/env
	ghcup config add-release-channel https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/ghcup-wasm-0.0.9.yaml
	ghcup install ghc wasm32-wasi-9.14 -- $CONFIGURE_ARGS

optim:
	wasm-opt -all -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

serve:
	http-server public

ghcup-serve:
	cd public; php -S localhost:1234

clean:
	rm -rf dist-newstyle public

update-js:
	cabal update --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

build-js:
	cabal build --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
	cp -v ./dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/*/x/app/build/app/app.jsexe/all.js .
	rm -rf public
	cp -rv static public
	bunx swc ./all.js -o public/index.js
