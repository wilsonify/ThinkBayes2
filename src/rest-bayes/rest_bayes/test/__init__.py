import logging
import os

import connexion
from flask_testing import TestCase
from json_ref_dict import materialize, RefDict

from rest_bayes.encoder import JSONEncoder

path_to_here = os.path.abspath(os.path.join(__file__, os.pardir))
path_to_there = os.path.abspath(os.path.join(__file__, os.pardir, os.pardir))


class BaseTestCase(TestCase):

    def create_app(self):
        openapi_refdict = RefDict(f"{path_to_there}/openapi/openapi.yaml")
        openapi_dict = materialize(openapi_refdict)
        logging.getLogger('connexion.operation').setLevel('ERROR')
        app = connexion.App(__name__, specification_dir='../openapi/')
        app.app.json_encoder = JSONEncoder
        app.add_api(openapi_dict, pythonic_params=True)
        return app.app
