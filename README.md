# [Class Template] Foundry Class Notes Repository

Welcome to the Foundry Repository for Class Notes. This README guides you through setting up and contributing to the class notes using Python Sphinx.

## Forking the Repository

**Important**: Please fork the current repository when creating a new class repository. This ensures you have the latest version of the setup and workflow.

## Sphinx Setup Adjustments

The repository comes pre-configured with Sphinx, but you need to make the following adjustments:

1. **Change the project title**:
   - Open `conf.py`.
   - Modify the line `project = 'TITLE'` to your class's relevant title.

2. **Update the index and documentation tree**:
   - Open `index.rst`.
   - Change the title to the relevant class name.
   - As you add documents to the project, ensure they are added to the doctree in `index.rst`.

## Using Markdown for Notes

The current setup allows you to use Markdown files for your notes. This is facilitated by the MyST Parser. Ensure your notes are in `.md` format and placed in the appropriate directory.

## Required Local Installations

Before starting, ensure you have the following packages installed locally:

- **Sphinx**: The primary tool for generating documentation.
- **MyST Parser (myst_parser)**: This allows you to write your documentation in Markdown.
- **Sphinx Book Theme**: Provides a visually appealing theme for your notes.

You may install them using the `requirements.txt` file included in the repository:

```bash
pip install -r requirements.txt
```

## GitHub Workflow for Publishing

A GitHub workflow is set up to publish your notes as pages. To make this work, follow these steps:

1. **Activate GitHub Pages:**
    - Go to `Settings > Pages > Build and deployment`.
    - Choose `Source` and select `GitHub Actions`.

2. **Build Locally Before Pushing:**
    - Ensure you have built your pages locally before pushing to the repository. This helps avoid build errors on GitHub.

## Useful Commands with Makefile

A makefile is included in the repository to streamline the documentation process. Here are some commands you might find useful:

- **Clean Docs:**
    ```
    clean-doc:
        rm -rf ./docs/build
        sphinx-build -M html docs/source/ docs/build/
    ```

- **Build Docs:**
    ```
    doc:
        sphinx-build -M html docs/source/ docs/build/
    ```

- **Push Changes:**
    ```
    push:
        git add .
        git commit -m "Minor push"
        git push
    ```

**Note:** It's important to familiarize yourself with Sphinx documentation and GitHub workflows to make the most out of this setup. Happy Documenting!
