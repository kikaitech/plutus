rm -f ./primary.db
stack exec plutus-pab -- --config=./config.yaml migrate
stack exec plutus-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/indigo-mint"
stack exec plutus-pab -- --config=./config.yaml all-servers
