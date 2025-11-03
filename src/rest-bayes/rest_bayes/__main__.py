#!/usr/bin/env python3
import os

import connexion

from rest_bayes import encoder

from json_ref_dict import RefDict, materialize

path_to_here = os.path.abspath(os.path.join(__file__, os.pardir))


def main():
    openapi_refdict = RefDict(f"{path_to_here}/openapi/openapi.yaml")
    openapi_dict = materialize(openapi_refdict)
    app = connexion.App(__name__)
    app.app.json_encoder = encoder.JSONEncoder
    
    # Add security headers for DAST compliance
    @app.app.after_request
    def add_security_headers(response):
        response.headers['X-Content-Type-Options'] = 'nosniff'
        response.headers['X-Frame-Options'] = 'DENY'
        response.headers['X-XSS-Protection'] = '1; mode=block'
        response.headers['Strict-Transport-Security'] = 'max-age=31536000; includeSubDomains'
        response.headers['Content-Security-Policy'] = "default-src 'self'"
        return response
    
    app.add_api(
        specification=openapi_dict,
        arguments={"title": "Swagger Petstore"},
        pythonic_params=True
    )

    # Run with debug disabled for production
    app.run(port=8080, debug=False)


if __name__ == '__main__':
    main()
