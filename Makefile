test:
	dotnet test

test-lispboot:
	dotnet run -p src/Fslisp.Driver -- -test lispboot/test

build:
	dotnet build -c Release

publish:
	dotnet publish src/Fslisp.Driver -c Release -o bin

publish-single-executable:
	dotnet publish src/Fslisp.Driver -c Release -o bin -r linux-x64 /p:PublishSingleFile=true /p:PublishTrimmed=true /p:PublishReadyToRun=true

