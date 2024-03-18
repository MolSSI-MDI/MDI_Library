"""
Script to generate engine reports
"""

import yaml


def load_engines():
    """Load engine information from engines.yaml"""

    with open("engines.yaml", "r", encoding='utf-8') as f:
        engines = yaml.safe_load(f)

    return engines

def create_engine_report(name, engine):
    """Create a report for a single engine"""
    # clone the engine report repo
    import subprocess

    # open the starting text
    with open(f"user_guide/mdi_ecosystem/starter/{name}.md", "r") as f:
        starting = f.read()

    # Clone the report
    try:
        subprocess.run(["git", "clone", f"{engine['mdimechanic_report']}", f"user_guide/mdi_ecosystem/reports/{name}"])
    except:
        pass

    # edit the README.md file
    with open(f"user_guide/mdi_ecosystem/reports/{name}/README.md", "r") as f:
        report_text = f.read()

    report_text = report_text.replace("# ", "## ")
    
    new_text = f"({name}-target)=\n"
    new_text += f"# {name}\n\n"

    new_text += starting + "\n\n"   
    new_text += report_text

    # overwrite the readme
    with open(f"user_guide/mdi_ecosystem/reports/{name}/README.md", "w") as f:
        f.write(new_text)
    

def start_pages(arg1=None, arg2=None):

    # delete all the old reports
    import shutil
    import os

    try:
        shutil.rmtree("user_guide/mdi_ecosystem/reports")
        # make a new reports directory
        os.mkdir("user_guide/mdi_ecosystem/reports")
    except:
        pass

    # load the engines
    engines = load_engines()    

    for name, engine in engines.items():
        create_engine_report(name, engine)
    


