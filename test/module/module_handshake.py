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

def module_handshake(ENDPOINTS, module_id, secret):
    """
    Performs the Challenge-Response handshake.
    Hits: handshake_handler.erl -> handle_challenge & handle_response
    """
    url = ENDPOINTS["handshake"]

    # Step 1: Request Challenge
    challenge_payload = {
        "handshake": "challenge",
        "module_id": module_id
    }

    try:
        resp1 = requests.post(url, json=challenge_payload)
        if resp1.status_code != 200:
            print(f"[Handshake] Challenge Request Failed: {resp1.text}")
            return None

        challenge_b64 = resp1.json().get("challenge")
        # In a real scenario, we decode b64, assume we sign the raw bytes, then re-encode
        # For this test, assuming the server expects the signature of the decoded challenge
        challenge_bytes = base64.b64decode(challenge_b64)

        # Calculate Response (Assuming HMAC of challenge using secret)
        # Note: You might need to adjust this depending on exact module_cache logic
        response_signature = calculate_signature(secret, challenge_bytes)

        # Step 2: Send Response
        response_payload = {
            "handshake": "response",
            "module_id": module_id,
            "response": response_signature
        }

        resp2 = requests.post(url, json=response_payload)

        if resp2.status_code == 200:
            token_data = resp2.json()
            print(f"[Handshake] Success. Token received.")
            return token_data.get("auth_token")
        else:
            print(f"[Handshake] Response Verification Failed: {resp2.text}")
            return None

    except Exception as e:
        print(f"[Handshake] Error: {e}")
        return None

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