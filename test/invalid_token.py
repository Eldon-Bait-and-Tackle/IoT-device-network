import requests
import sys


def invalid_token(API_URL):
    print("\n Testing API with FAKE Token (Expecting 401)...")

    headers = {'Authorization': 'Bearer fake.token.123'}
    params = {'request': 'get_module', 'module_id': '1'}

    response = requests.get(API_URL, params=params, headers=headers)

    if response.status_code == 401:
        print("✅ PASS: API correctly rejected the fake token.")
    else:
        print(f"❌ FAIL: API let a fake token in! Status: {response.status_code}")
        