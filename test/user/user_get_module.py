import requests
import sys

def get_module(API_URL, token, module_number):
    print("\n Testing GET /?request=get_module with Valid Token...")

    headers = {
        'Authorization': f'Bearer {token}'
    }

    params = {
        'request': 'get_module',
        'module_id': '{module_number}'
    }

    try:
        response = requests.get(API_URL, params=params, headers=headers)

        if response.status_code == 200:
            print(f"+ PASS: API accepted the token. Response: {response.json()}")
            return
            
        elif response.status_code == 404:
            print(f"= PASS: API accepted the token but no module was found.")
            return
            
        print(f"- FAIL: API rejected valid token. Status: {response.status_code}")
        print(f"   Body: {response.text}")

    except Exception as e:
        print(f"- Connection Error: {e}")