.PHONY: all test clean

BUCKET_NAME := "jointhefreeworld.org"

fmt:
	find . -name '*.hs' -type f -exec ormolu --mode inplace {} \;
	find . -name '*.nix' -exec nixfmt {} \;
	-statix check
	-deadnix -f

clean:
	rm -rfv dist


build: clean fmt
# Home and Blog
	stack run
	cp -rfv resources dist/
	cp -rfv dist/home/* dist/
	rm -rfv dist/home
# CV
	mkdir dist/cv
	cp projects/cv/index.html dist/cv/
# Minify
	minify -r dist -o dist-min
	rm -rfv dist
	mv dist-min/dist dist
	rm -rfv dist-min

dev: build
# watchexec -r -e hs,org,css,js "make build && simple-http-server --nocache -i -p 8989 ./dist"
	simple-http-server --nocache -i -p 8989 ./dist
publish:
	aws s3 rm --recursive "s3://$(BUCKET_NAME)/*"
	aws s3 cp --recursive "dist/" "s3://$(BUCKET_NAME)/"
