.PHONY: devel hlint stylish-haskell test

DB_HOST ?= localhost
DB_PORT ?= 5555
DB_NAME ?= flipper
DB_USER ?= flipper
DB_PASS ?= flipper123
DB_POOL_SIZE ?= 1

devel:
	DB_HOST=$(DB_HOST) \
	DB_PORT=$(DB_PORT) \
		DB_NAME=$(DB_NAME) \
		DB_USER=$(DB_USER) \
		DB_PASS=$(DB_PASS) \
		DB_POOL_SIZE=$(DB_POOL_SIZE) \
		stack test --pedantic --file-watch --exec="$(MAKE) hlint"

test:
	DB_HOST=$(DB_HOST) \
	DB_PORT=$(DB_PORT) \
		DB_NAME=$(DB_NAME) \
		DB_USER=$(DB_USER) \
		DB_PASS=$(DB_PASS) \
		DB_POOL_SIZE=$(DB_POOL_SIZE) \
		stack test --pedantic --exec="$(MAKE) hlint"

hlint:
	hlint src

stylish-haskell:
	find ./test -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i && \
		find ./src -name "*.hs" | xargs stylish-haskell -c .stylish-haskell.yaml -i
