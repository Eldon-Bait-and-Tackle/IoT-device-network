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


def test_transmission(ENDPOINTS, module_id, secret, temp, moisture, battery):
    """
    Tests the module's transmission capabilities.
    Hits: transmission_handler.erl
    """
    url = ENDPOINTS["transmission"]

    # Data to sign (Concatenated based on handler logic: <<Mid, Temp, Moist, Bat>>)
    # Note: Ensure these are strings before concatenation
    data_to_sign = f"{module_id}{temp}{moisture}{battery}"

    signature = calculate_signature(secret, data_to_sign)

    # transmission_handler uses uri_string:dissect_query, so we send Form-Encoded data
    payload = {
        "module_id": module_id,
        "temperature": temp,
        "moisture": moisture,
        "battery": battery,
        "signature": signature
    }

    try:
        response = requests.post(url, data=payload)
        print(f"[Transmission] Sent: {payload} | Response: {response.status_code} {response.text}")
        return response.text
    except Exception as e:
        print(f"[Transmission] Error: {e}")
        return str(e)
    
    
    
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
