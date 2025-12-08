import requests
import sys

def get_token(KC_URL, USERNAME, REALM, CLIENT_ID, PASSWORD):
    print(f"\n Authenticating with Keycloak as '{USERNAME}'...")

    token_url = f"{KC_URL}/realms/{REALM}/protocol/openid-connect/token"
    payload = {
        'client_id': CLIENT_ID,
        'username': USERNAME,
        'password': PASSWORD,
        'grant_type': 'password',
        'scope': 'openid'
    }

    try:
        response = requests.post(token_url, data=payload)
        response.raise_for_status()
        data = response.json()
        access_token = data['access_token']
        print("✅ Successfully retrieved Access Token!")
        print(access_token)
        return access_token
    except requests.exceptions.RequestException as e:
        print(f"❌ Failed to login to Keycloak: {e}")
        if response is not None:
            print(f"   Response: {response.text}")
        sys.exit(1)