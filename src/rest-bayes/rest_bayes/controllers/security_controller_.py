from typing import List


def info_from_api_key(api_key, required_scopes):
    """
    Check and retrieve authentication information from api_key.
    Returned value will be passed in 'token_info' parameter of your operation function, if there is one.
    'sub' or 'uid' will be set in 'user' parameter of your operation function, if there is one.

    :param api_key API key provided by Authorization header
    :type api_key: str
    :param required_scopes Always None. Used for other authentication method
    :type required_scopes: None
    :return: Information attached to provided api_key or None if api_key is invalid or does not allow access to called API
    :rtype: dict | None
    """
    # Validate API key format and length
    if not api_key or len(api_key) < 16:
        return None
    
    # For demo purposes, accept 'special-key' as mentioned in OpenAPI spec
    if api_key == 'special-key':
        return {'uid': 'demo_user'}
    
    return None


def info_from_petstore_auth(token):
    """
    Validate and decode token.
    Returned value will be passed in 'token_info' parameter of your operation function, if there is one.
    'sub' or 'uid' will be set in 'user' parameter of your operation function, if there is one.
    'scope' or 'scopes' will be passed to scope validation function.

    :param token Token provided by Authorization header
    :type token: str
    :return: Decoded token information or None if token is invalid
    :rtype: dict | None
    """
    # Basic token validation
    if not token or len(token) < 10:
        return None
    
    # For demo purposes, return mock token info
    return {'scopes': ['read:pets', 'write:pets'], 'uid': 'demo_user'}


def validate_scope_petstore_auth(required_scopes, token_scopes):
    """
    Validate required scopes are included in token scope

    :param required_scopes Required scope to access called API
    :type required_scopes: List[str]
    :param token_scopes Scope present in token
    :type token_scopes: List[str]
    :return: True if access to called API is allowed
    :rtype: bool
    """
    if not required_scopes or not token_scopes:
        return False
    
    return set(required_scopes).issubset(set(token_scopes))

