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

def register_module(ENDPOINTS, secret):
    """
    Registers a module with the given secret.
    Hits: handshake_handler.erl -> handle_registration
    """
    url = ENDPOINTS["handshake"]
    payload = {
        "handshake": "register",
        "secret": secret
    }

    try:
        response = requests.post(url, json=payload)

        if response.status_code == 200:
            data = response.json()
            module_id = str(data.get("module_id"))
            print(f"[Register] Success: Assigned Module ID {module_id}")
            return module_id
        else:
            print(f"[Register] Failed: {response.status_code} - {response.text}")
            return None
    except Exception as e:
        print(f"[Register] Error: {e}")
        return None