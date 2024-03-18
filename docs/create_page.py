
"""
Functions to generate pages for MDI Standard
"""

import yaml
from collections import OrderedDict
from pathlib import Path 
import shutil

def load_standard():
    # Load the MDI standard
    with open("mdi_standard.yaml", "r", encoding='utf-8') as f:
        mdi_standard = yaml.safe_load(f)

    return mdi_standard["categories"], mdi_standard["commands"]

def group_commands(categories,standard):
    """Groups sending and receiving commands together and then sorts them alphabetically"""

    grouped_standard = {}
    for command, command_info in standard.items():
        if command.startswith("<") or command.startswith(">"):
            if command[1] !="@" and not command[1].isalpha():
                command_group = command[2:]
            else:
                command_group = command[1:]  
        else:   
            command_group = command
            
        command_info["name"] = command
        
        if command_group not in grouped_standard:
            grouped_standard[command_group] = []
        
        grouped_standard[command_group].append(command_info)

    # Sorting each group's commands alphabetically by their 'name' key
    for group in grouped_standard:
        grouped_standard[group] = sorted(grouped_standard[group], key=lambda x: x['name'])
    
    # Sorting the groups alphabetically and returning an OrderedDict
    ordered_grouped_standard = OrderedDict(sorted(grouped_standard.items()))

    category_groups = {}
    for category_id, category_info in categories.items():
        category_groups[category_id] = {"name": category_info["name"], "commands": OrderedDict()}

    for group_name, group_commands in ordered_grouped_standard.items():
        # Assume grouped commands have the same category
        # hopefully this is always the case
        category_id = group_commands[0]["category"]

        category_groups[category_id]["commands"][group_name] = group_commands

    return category_groups

def create_page(command_name, command_list):

    # Create a new page for the command
    page_text = f"# {command_name}\n\n"

    for command_dict in command_list:

        page_text += f"\n\n## {command_dict['name']}\n\n"

        tags = command_dict.get("tags")

        if tags:
            page_text += "```{tags}"
            for tag in tags:
                page_text += f" {tag},"
            
            page_text = page_text[:-1]
            page_text += "\n```\n\n"

        page_text += f"{command_dict['description']}\n"

        if command_dict.get("datatype"):
            page_text += f"\n**Datatype:** `{command_dict['datatype']}`  "
            page_text += f"\n**Quantity**: `{command_dict['count']}`  "

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
    index_text = "# MDI Standard by Category\n\n"
    index_text += "This section provides details on the commands defined by the MDI Standard."
    index_text += "The MDI Standard defines a set of commands that can be used to control simulations and communicate data between engines."

    index_text += "\n\n## Command Categories\n\n"
    
    list_section = ""
    toc_section = ""
    for _, category_info in categories.items():
        category_slug = category_info["slug"]
        category_name = category_info["name"]
        list_section += f"**- {category_name}**: {category_info['description']}  \n"
        toc_section += f"commands/{category_slug}/index\n"

    index_text += list_section + "\n\n"

    index_text += "```{toctree}\n:hidden:\n:maxdepth:3\n\n" + toc_section + "```\n"

    return index_text



def generate_api_pages(arg1=None, arg2=None):
    # remove commands directory
    shutil.rmtree("api/mdi_standard/commands", ignore_errors=True)

    command_categories, commands_list = load_standard()
    grouped_commands = group_commands(command_categories, commands_list)

    # Create index page
    index_text = create_main_index(command_categories)
    with open("api/mdi_standard/index.md", "w") as f:
        f.write(index_text)

    for category_id, category_info in command_categories.items():
        category_slug = category_info["slug"]
        category_name = category_info["name"]
        Path(f"api/mdi_standard/commands/{category_slug}").mkdir(parents=True, exist_ok=True)

        # Create the category index page
        with open(f"api/mdi_standard/commands/{category_slug}/index.md", "w") as f:
            f.write(f"# {category_name}\n\n{category_info['description']}\n\n")
            f.write("Command Name | Description\n")
            f.write("------------ | -----------\n")

        table_text = ""
        toc_text = "```{toctree}\n:hidden:\n\n"
        for command_name, command_list in grouped_commands[category_id]["commands"].items():
            page_text = create_page(command_name, command_list)
            table_text += f"[{command_name}]({command_name}.md) | {command_list[0]['description']}\n"
            toc_text += f"{command_name}\n"

            with open(f"api/mdi_standard/commands/{category_slug}/{command_name}.md", "w+") as f:
                f.write(page_text)

        # Add to the category index page
        with open(f"api/mdi_standard/commands/{category_slug}/index.md", "a") as f:
            f.write(table_text)
            f.write("\n\n<!--Make a TOC for the sidebar and so Sphinx doesn't complain -->\n<!-- These comments are necessary to break up the table and the TOC -->\n\n")
            f.write(toc_text)
            f.write("```\n")

