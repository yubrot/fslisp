test:
	dotnet test

test-rosetta-lisp:
	dotnet run -p src/Fslisp.Driver -- -test rosetta-lisp/test

build:
	dotnet build -c Release

publish:
	dotnet publish src/Fslisp.Driver -c Release -o bin

publish-single-executable:
	dotnet publish src/Fslisp.Driver -c Release -o bin -r linux-x64 /p:PublishSingleFile=true /p:PublishTrimmed=true /p:PublishReadyToRun=true

