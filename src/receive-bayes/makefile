
all: docker-build	

docker-build:
	docker build -t recieve-bayes .

docker-run: docker-build
	docker run --rm --name recieve-bayes --env AMQP_HOST=thinkpad recieve-bayes
