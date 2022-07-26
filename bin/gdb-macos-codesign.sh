#!/bin/bash

set -ex

URL="https://sourceware.org/gdb/wiki/PermissionsDarwin"
ENTITLEMENTS="$(mktemp)"

if ! security find-certificate -c gdb-cert | grep System.keychain; then
	echo "Cert not found, see $URL"
	exit 1
fi

if ! security find-certificate -p -c gdb-cert | openssl x509 -checkend 0; then
	echo "Certificate found but expired, see $URL"
	exit 1
fi

if ! security dump-trust-settings -d | grep gdb-cert -A 3 | tail -n1 | grep 'Code Signing'; then
	echo "Certificate found and not expired, but no Code Signing capability, see $URL"
fi

cat <<'EOF' > "$ENTITLEMENTS"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>com.apple.security.cs.debugger</key>
    <true/>
</dict>
</plist>
EOF

codesign --entitlements "$ENTITLEMENTS" -fs gdb-cert "$(which gdb)"
rm "$ENTITLEMENTS"
sudo killall taskgated
