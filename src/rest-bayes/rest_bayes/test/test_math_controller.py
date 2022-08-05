# coding: utf-8

from __future__ import absolute_import

import unittest

from flask import json

from rest_bayes.test import BaseTestCase


class TestMathController(BaseTestCase):
    """MathController integration test stubs"""

    def test_sqrt(self):
        """Test case for sqrt"""
        sqrt_input = {"x": 0}
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/sqrt',
            method='POST',
            headers=headers,
            data=json.dumps(sqrt_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_strength(self):
        """Test case for strength"""
        strength_input = {"actual": 60, "expected": 100}
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/strength',
            method='POST',
            headers=headers,
            data=json.dumps(strength_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_conjunction(self):
        """Test case for conjunction"""
        conjunction_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/conjunction',
            method='POST',
            headers=headers,
            data=json.dumps(conjunction_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_conditional(self):
        """Test case for conditional"""
        conditional_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/conditional',
            method='POST',
            headers=headers,
            data=json.dumps(conditional_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_bayes(self):
        """Test case for bayes"""
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/bayes',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )


if __name__ == '__main__':
    unittest.main()
