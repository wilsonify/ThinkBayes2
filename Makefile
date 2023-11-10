PROJECT_NAME = ThinkBayes2
PYTHON_VERSION = 3.10
PYTHON_INTERPRETER = python

all: clean test

create_environment:
	conda create -y --name $(PROJECT_NAME) python=$(PYTHON_VERSION) pymc
	@echo ">>> conda env created. Activate with:\nconda activate $(PROJECT_NAME)"


requirements:
	$(PYTHON_INTERPRETER) -m pip install -r requirements.txt
	$(PYTHON_INTERPRETER) -m pip install -r requirements-dev.txt
	$(PYTHON_INTERPRETER) -m pip install src/think-bayes



clean:
	echo "Delete all compiled Python files"
	find . -type f -name "*.py[co]" -delete
	find . -type d -name "__pycache__" -delete


test:
	pytest tests
