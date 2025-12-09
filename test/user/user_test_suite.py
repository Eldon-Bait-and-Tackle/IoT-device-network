import requests
import sys

from get_token import get_token
from user_get_module import get_module
from invalid_token import invalid_token
from user_claim_module import claim_module


KC_URL = "http://100.73.81.46:8080"
REALM = "hsn_kc"
CLIENT_ID = "public_client" 
USERNAME = "eldon"
PASSWORD = "admin1"

API_URL = "http://100.73.81.46:8082/api"


SECRET = "tmp_123" # AIWJDIOAWIODJIOAWODIIOAWD change this later this is for a single module

token_url = f"{KC_URL}/realms/{REALM}/protocol/openid-connect/token"




token = get_token(KC_URL, USERNAME, REALM, CLIENT_ID, PASSWORD)

invalid_token(API_URL)
claim_module(API_URL, token, SECRET)
get_module(API_URL, token, 10)






