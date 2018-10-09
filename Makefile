# Top level build file for ScotTraffic Server Tools
	
CERT_DIR=etc/apns-certs
CERTS=$(CERT_DIR)/apns-cert-dev.pem \
	$(CERT_DIR)/apns-key-dev.pem \
	$(CERT_DIR)/apns-cert-production.pem \
	$(CERT_DIR)/apns-key-production.pem

all: stack certs

clean: clean-stack clean-certs
	
certs: $(CERTS)

$(CERT_DIR)/apns-cert-%.pem: $(CERT_DIR)/%.p12
	openssl pkcs12 -in $< -out $@ -clcerts -nokeys

$(CERT_DIR)/apns-key-%.pem: $(CERT_DIR)/%.p12
	openssl pkcs12 -in $< -out $@ -nocerts -nodes

clean-certs:
	-rm $(CERT_DIR)/*.pem 2>/dev/null

stack:
	stack setup
	stack build

install: stack
	mkdir -p bin
	stack --local-bin-path=bin install

clean-legacy:
	-rm -rf .cabal-sandbox cabal.sandbox.config
	-find src -name dist -exec rm -rf {} \;
	-find src -name cabal.sandbox.config -exec rm -f {} \;

clean-stack:
	stack clean

