all: build

build:
	dotnet publish --configuration Release
	ln -s ./bin/Release/net7.0/publish/Gerald.ConsoleApp ./bin/Gerald
