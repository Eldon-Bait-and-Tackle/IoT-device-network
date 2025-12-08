import requests
import sys


def claim_module(API_URL, token, secret):
    print("\n Testing POST claim_device with Valid Token...")

    headers = {
        'Authorization': f'Bearer {token}',
        'Content-Type': 'application/json'
    }

    data = {
        'request': 'claim_device',
        'secret': str(secret)
    }

    try:
        response = requests.post(
            API_URL,
            params={'request': 'claim_device'},
            json=data,
            headers=headers
        )

        if response.status_code == 200:
            print(f"✅ PASS: Device claimed. Response: {response.json()}")
        elif response.status_code == 400 and "Claim failed" in response.text:
            print(f"✅ PASS: Auth worked (Application logic returned error): {response.text}")
        else:
            print(f"❌ FAIL: Unexpected status: {response.status_code}")
            print(f"   Body: {response.text}")

    except Exception as e:
        print(f"❌ Connection Error: {e}")