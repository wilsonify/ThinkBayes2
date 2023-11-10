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
    app.add_api(
        specification=openapi_dict,
        arguments={"title": "Swagger Petstore"},
        pythonic_params=True
    )

    app.run(port=8080)


if __name__ == '__main__':
    main()
