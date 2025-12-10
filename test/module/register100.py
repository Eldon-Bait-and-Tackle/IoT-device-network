import requests
import json
import secrets
import hmac
import hashlib
import random
import base64
import time
import concurrent.futures
from urllib.parse import urlencode


from module_registration import register_module


BASE_URL = "http://100.73.81.46:8082"
ENDPOINTS = {
    "api": f"{BASE_URL}/api",
    "handshake": f"{BASE_URL}/handshake",
    "transmission": f"{BASE_URL}/transmission"
}
CREDENTIALS_FILE = "module_credentials.txt"

def generate_secret():
    """Generates a secure random hex string to use as a secret."""
    return secrets.token_hex(32)


def save_credential(module_id, secret):
    """Appends the module_id and secret to a text file."""
    with open(CREDENTIALS_FILE, "a") as f:
        f.write(f"{module_id},{secret}\n")
    print(f"[Storage] Saved credentials for Module {module_id}")

def calculate_signature(secret, data_bytes):
    """
    Calculates an HMAC-SHA256 signature.
    Matches logic implied by: module_cache:verify_transmission
    """
    if isinstance(secret, str):
        secret = secret.encode('utf-8')
    if isinstance(data_bytes, str):
        data_bytes = data_bytes.encode('utf-8')

    signature = hmac.new(secret, data_bytes, hashlib.sha256).hexdigest()
    return signature


# ==========================================

print("=== Module Registration Test: 100 Modules ===")

for i in range(100):
    secret = generate_secret()

    module_id = register_module(ENDPOINTS, secret)

    if module_id:
        save_credential(module_id, secret)
    else:
        print(f"[Test] Registration failed for module {i+1}")
        
    print(module_id, secret)

