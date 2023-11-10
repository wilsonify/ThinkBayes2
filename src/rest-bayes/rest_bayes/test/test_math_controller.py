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
            '/sqrt',
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
            '/strength',
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
            '/conjunction',
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
            '/conditional',
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
            '/bayes',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_cookie(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/cookie',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        assert response.get_json() == {
            'a': [0, 0, 0, 0, 1, 0, 1, 0],
            'b': [0, 0, 0, 0, 1, 0, 0, 1],
            'strategy': 'cookie'
        }
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_monty(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/cookie',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        assert response.get_json() == {}
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_mandm(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/mandm',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        assert response.get_json() == {'a': [0, 0, 0, 0, 1, 0, 1, 0], 'b': [0, 0, 0, 0, 1, 0, 0, 1],
                                       'strategy': 'cookie'}
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_euro(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/euro',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        assert response.get_json() == {'a': [0, 0, 0, 0, 1, 0, 1, 0], 'b': [0, 0, 0, 0, 1, 0, 0, 1],
                                       'strategy': 'cookie'}
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_odds_to_probability(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/odds_to_probability',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        assert response.get_json() == {
            'a': [0, 0, 0, 0, 1, 0, 1, 0],
            'b': [0, 0, 0, 0, 1, 0, 0, 1],
            'strategy': 'cookie'
        }
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_probability_to_odds(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/probability_to_odds',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')

        assert response.get_json() == {'a': [0, 0, 0, 0, 1, 0, 1, 0], 'b': [0, 0, 0, 0, 1, 0, 0, 1],
                                       'strategy': 'cookie'}
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_blood(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/blood',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_hockey(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/hockey',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_overtime(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/overtime',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_overtime2(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/overtime2',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_shut_out(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/shut_out',
            method='POST',
            headers=headers,
            data=json.dumps(bayes_input),
            content_type='application/json')
        self.assert200(
            response=response,
            message='Response body is : ' + response.data.decode('utf-8')
        )

    def test_soccer(self):
        bayes_input = {
            "a": [0, 0, 0, 0, 1, 0, 1, 0],
            "b": [0, 0, 0, 0, 1, 0, 0, 1]
        }
        headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/soccer',
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
