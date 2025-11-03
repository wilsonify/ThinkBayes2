PROJECT_NAME = ThinkBayes2
PYTHON_VERSION = 3.13
PYTHON_INTERPRETER = uv run python

all: clean test

create_environment:
	uv venv --python $(PYTHON_VERSION)
	uv pip install -e .[all]
	@echo ">>> uv environment created. Activate with:\nsource .venv/bin/activate"

requirements:
	uv pip install -e .[all]

clean:
	echo "Delete all compiled Python files"
	find . -type f -name "*.py[co]" -delete
	find . -type d -name "__pycache__" -delete
	find . -type d -name "*.egg-info" -exec rm -rf {} +
	rm -rf build/
	rm -rf dist/

test:
	uv run pytest tests

install-dev:
	uv pip install -e .[dev]

install-all:
	uv pip install -e .[all]

lint:
	uv run ruff check .
	uv run black --check .

format:
	uv run ruff check . --fix
	uv run black .

build:
	uv build

.PHONY: all create_environment requirements clean test install-dev install-all lint format build
