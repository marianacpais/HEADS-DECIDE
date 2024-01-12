clean-doc:
	rm -rf ./docs/build
	sphinx-build -M html docs/source/ docs/build/

doc:
	sphinx-build -M html docs/source/ docs/build/

push:
	git add .
	git commit -m "Minor push"
	git push