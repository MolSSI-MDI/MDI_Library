
"""
Functions to generate pages for MDI Standard
"""

import yaml
from collections import OrderedDict
from pathlib import Path 
import shutil
import warnings

def load_standard():
    # Load the MDI standard
    with open("command_pages.yaml", "r", encoding='utf-8') as f:
        mdi_standard = yaml.safe_load(f)
    
    with open("categories.yaml") as f:
        categories = yaml.safe_load(f)

    return categories, mdi_standard

def group_commands(categories, standard):
    """Groups sending and receiving commands together and then sorts them alphabetically"""

    # Prepare data structure for holding commands by group.
    category_groups = {}
    for category_id, category_info in categories.items():
        category_groups[category_id] = {"name": category_info["name"], "commands": OrderedDict()}
    
    # Sort commands alphabetically
    sorted_commands = sorted(standard.keys())

    # Group commands by category
    for command in sorted_commands:
        try:
            category_id = standard[command]["category"]
        except:
            warnings.warn(f"No category for {command}.")
            # Just put in data exchange for now.
            category_id = 2

        
        category_groups[category_id]["commands"][command] = standard[command]

    return category_groups

def create_page(command_group_info, command_list):

    command_name = command_group_info["name"]
    tags = command_group_info["tags"]

    # Create a new page for the command
    page_text = f"({command_name}-target)=\n"
    page_text += f"# {command_name}\n"

    if tags:
        page_text += "```{tags}"
        for tag in tags:
            page_text += f" {tag},"
        
        page_text = page_text[:-1]
        page_text += "\n```\n\n"

    page_text += f"{command_group_info['summary']}\n\n"

    for command_info in command_list:
        for command, command_dict in command_info.items():
            page_text += f"\n\n## {command}\n\n"
        
            page_text += f"{command_dict.get('description')}\n"

            if command_dict.get("datatype"):
                page_text += f"\n**Datatype:** `{command_dict['datatype']}`  "
                page_text += f"\n**Quantity**: `{command_dict['count']}`  "
            
            if command_dict.get("units"):
                page_text += f"\n**Units:** {command_dict['units']}\n"

            if command_dict.get("format"):
                page_text += f"\n**Format:** {command_dict['format']}"

            page_text += f"\n\n {command_dict.get('doc')}"

            if command_dict.get("admonition"):
                admonition_info = command_dict["admonition"]

                page_text += f"\n\n:::{{admonition}} {admonition_info['title']}"
                page_text += f"\n:class: {admonition_info['type']}"
                page_text += f"\n\n{admonition_info['content']}"
                page_text += "\n:::"

            if command_dict.get("examples"):
                page_text += "\n\n### Examples\n"

                page_text += "\n\n::::{tab-set}"
                page_text += "\n\n:::{tab-item} Python"
                page_text += "\n:sync: python"
                page_text += "\n\n```python"
                page_text += f"\n{command_dict['examples']['python']}"
                page_text += "\n```\n:::"

                if command_dict["examples"].get("python (numpy)"):
                    page_text += "\n\n:::{tab-item} Python (NumPy)"
                    page_text += "\n:sync: python (numpy)"
                    page_text += "\n\n```python"
                    page_text += f"\n{command_dict['examples']['python (numpy)']}"
                    page_text += "\n```\n:::"

                page_text += "\n\n:::{tab-item} C++"
                page_text += "\n:sync: cpp"
                page_text += "\n\n```cpp"
                page_text += f"\n{command_dict['examples']['cpp']}"
                page_text += "\n```\n:::"

                page_text += "\n\n::::"                                                             


    return page_text

def create_main_index(categories):
    index_text = "# MDI Standard\n\n"
    index_text += "This section provides details on the commands defined by the MDI Standard."
    index_text += "The MDI Standard defines a set of commands that can be used to control simulations and communicate data between engines.\n\n"
    
    index_text += "You can also [tags](../../_tags/tagsindex) for the MDI Standard.\n\n"

    list_section = ""
    toc_section = ""
    for _, category_info in categories.items():
        category_slug = category_info["slug"]
        toc_section += f"commands/{category_slug}/index\n"

    toc_section += "../../_tags/tagsindex\n"
    index_text += list_section + "\n\n"

    index_text += "```{toctree}\n:hidden:\n\n" + toc_section + "```\n"
    
    return index_text

def create_command_tables(category_slug, category_info):
    #table_text = f"{category_info['description']}\n\n"
    table_text = "Command Name | Description\n"
    table_text += "------------ | -----------\n"
    toc_section = ""

    for command_name, command_info in category_info.items():
        table_text += f"[{command_name}](commands/{category_slug}/{command_name}) | {command_info['summary']}\n"
        toc_section += f"{command_name}\n"

    return table_text, toc_section

def generate_api_pages(arg1=None, arg2=None):
    # remove commands directory
    shutil.rmtree("api/mdi_standard/commands", ignore_errors=True)

    command_categories, commands_list = load_standard()
    grouped_commands = group_commands(command_categories, commands_list)

    # Create index page
    index_text = create_main_index(command_categories)
    
    # Loop through commands for category - put in table and make API page
    for category_id, category_info in grouped_commands.items():

        category_slug = command_categories[category_id]["slug"]
        category_description = command_categories[category_id]["description"]

        index_text += f"\n\n## {category_info['name']}\n\n"
        index_text += f"{category_description}\n\n"

        # Create a table of commands
        table_text, toc_section = create_command_tables(category_slug, category_info["commands"])

        index_text += table_text

        category_slug = command_categories[category_id]["slug"]
        category_name = command_categories[category_id]["name"]
        category_description = command_categories[category_id]["description"]

        # make a folder for the category
        Path(f"api/mdi_standard/commands/{category_slug}").mkdir(parents=True, exist_ok=True)

        # Create the category index page
        category_commands = category_info["commands"]
        
        for command_name, command_info in category_commands.items():

            command_group_info = {
                "name": command_name,
                "tags": command_info.get("tags"),
                "summary": command_info["summary"]
            }
            
            page_text = create_page(command_group_info, command_info["commands"])
            with open(f"api/mdi_standard/commands/{category_slug}/{command_name}.md", "w+") as f:
                f.write(page_text)

        with open(f"api/mdi_standard/commands/{category_slug}/index.md", "w") as f:
            f.write(f"# {category_name}\n\n{category_description}\n\n")
            f.write(table_text.replace(f"commands/{category_slug}/", ""))
            f.write("\n\n```{toctree}\n:hidden:\n\n")
            f.write(toc_section)

    # Write the index page
    with open("api/mdi_standard/index.md", "w") as f:
        f.write(index_text)
    
    
        

       

 





