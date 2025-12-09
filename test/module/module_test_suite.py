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


from moudle_transmission import test_transmission
from module_registration import register_module
from module_handshake import module_handshake

# ==========================================
# CONFIGURATION
# ==========================================
BASE_URL = "http://100.73.81.46:8082"
ENDPOINTS = {
    "api": f"{BASE_URL}/api",
    "handshake": f"{BASE_URL}/handshake",
    "transmission": f"{BASE_URL}/transmission"
}

# FILE TO STORE CREDENTIALS
CREDENTIALS_FILE = "module_credentials.txt"


# ==========================================
# HELPER FUNCTIONS (CRYPTO & UTILS)
# ==========================================

def generate_secret():
    """Generates a secure random hex string to use as a secret."""
    return secrets.token_hex(32)


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


def save_credential(module_id, secret):
    """Appends the module_id and secret to a text file."""
    with open(CREDENTIALS_FILE, "a") as f:
        f.write(f"{module_id},{secret}\n")
    print(f"[Storage] Saved credentials for Module {module_id}")


def load_credentials():
    """Loads all credentials from the file."""
    creds = []
    try:
        with open(CREDENTIALS_FILE, "r") as f:
            for line in f:
                parts = line.strip().split(',')
                if len(parts) == 2:
                    creds.append({"module_id": parts[0], "secret": parts[1]})
    except FileNotFoundError:
        print("[Storage] No credentials file found.")
    return creds




# ==========================================
# TEST FUNCTIONS HERE
# ==========================================




print("\n--- Module Test Suite ---")


sec = generate_secret()
Mid = register_module(ENDPOINTS, sec)
save_credential(Mid, sec)

creds = load_credentials()

if not creds:
    print("No credentials found. Run 'run_single_suite' first.")

for c in creds:
    temp = str(random.randint(10, 25))
    moist = str(random.randint(10, 25))
    bat = str(random.randint(10, 100))

    outcome = test_transmission(ENDPOINTS, c["module_id"], c["secret"], temp, moist, bat)
    print(outcome)

print("--- Stress Test Complete ---\n")


print("--- Single Suite Test Complete ---\n")











# ==========================================
# ORCHESTRATION FUNCTIONS
# ==========================================

def run_single_suite(secret=None):
    """
    Runs a full lifecycle test for a single new module.
    1. Generates Secret (if not provided)
    2. Registers
    3. Handshakes (Optional check)
    4. Transmits Data
    5. Saves Credentials
    """
    print("\n--- Starting Single Suite Test ---")
    if not secret:
        secret = generate_secret()
        print(f"Generated Secret: {secret}")

    # 1. Register
    module_id = register_module(secret)
    if not module_id:
        return

    # 2. Handshake (Optional functionality check)
    auth_token = module_handshake(module_id, secret)

    # 3. Transmission
    # Simulating sensor data
    test_transmission(module_id, secret, "25.5", "60", "98")

    # 4. Save
    save_credential(module_id, secret)

    # 5. Check API (Optional)
    if auth_token:
        get_public_data("get_module", module_id, auth_token)

    print("--- Single Suite Test Complete ---\n")


def stress_test_transmissions(count):
    """
    Loads saved modules and sends 'count' transmissions concurrently.
    """
    print(f"\n--- Starting Stress Test ({count} transmissions) ---")
    creds = load_credentials()

    if not creds:
        print("No credentials found. Run 'run_single_suite' first.")
        return

    def _single_task(i):
        # Pick a random module from our saved list (round-robin or random)
        cred = creds[i % len(creds)]
        # Randomize data slightly
        temp = str(20 + (i % 10))
        moist = str(50 + (i % 5))
        bat = str(100 - (i % 20))

        return test_transmission(cred["module_id"], cred["secret"], temp, moist, bat)

    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        futures = [executor.submit(_single_task, i) for i in range(count)]
        for future in concurrent.futures.as_completed(futures):
            pass  # Just waiting for completion

    print("--- Stress Test Complete ---\n")
