$certStorePath = "$env:USERPROFILE\Documents\CertStore"
if (-not (Test-Path $certStorePath)) {
    New-Item -ItemType Directory -Path $certStorePath | Out-Null
}

# Export the highest-level root certificate to the certificate store
$cert = Get-ChildItem -Path Cert:\CurrentUser\Root | Sort-Object -Property NotBefore | Select-Object -Last 1
$certPath = Join-Path -Path $certStorePath -ChildPath ($cert.Thumbprint + ".cer")
$cert | Export-Certificate -FilePath $certPath -Type CERT

# Set the environment variables for Python, Node.js, and OpenSSL
[System.Environment]::SetEnvironmentVariable("SSL_CERT_FILE", $certPath, [System.EnvironmentVariableTarget]::User)
[System.Environment]::SetEnvironmentVariable("NODE_EXTRA_CA_CERTS", $certPath, [System.EnvironmentVariableTarget]::User)
$opensslCnfPath = "$env:USERPROFILE\Documents\openssl.cnf"
$opensslCnfContent = "openssl_conf = openssl_init"
$opensslCnfContent += "`n`n"
$opensslCnfContent += "[openssl_init]`n"
$opensslCnfContent += "ssl_conf = ssl_sect`n`n"
$opensslCnfContent += "[ssl_sect]`n"
$opensslCnfContent += "system_default = system_default_sect`n`n"
$opensslCnfContent += "[system_default_sect]`n"
$opensslCnfContent += "CACertFile = $certPath"
$opensslCnfContent | Out-File -FilePath $opensslCnfPath

# Set the OPENSSL_CONF environment variable
[System.Environment]::SetEnvironmentVariable("OPENSSL_CONF", $opensslCnfPath, [System.EnvironmentVariableTarget]::User)

Write-Host "Certificate store created and environment variables set. Restart the applications to apply the changes."
