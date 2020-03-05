from setuptools import setup

def readme():
    try:
        with open('README.md') as f:
            return f.read()
    except IOError:
        return ''


setup(
    name='thinkbayes',
    version='2.0.0',
    author='Allen B. Downey',
    author_email='downey@allendowney.com',
    packages=['thinkbayes'],
    url='http://github.com/AllenDowney/ThinkBayes2',
    license='LICENSE.txt',
    description='Supporting code for the book Think Bayes 2e.',
    long_description=readme(),
)
