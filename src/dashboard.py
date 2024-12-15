from . import TASK_FILE
from flask import (
    Blueprint, jsonify, request
)

nya = Blueprint('nya', __name__)

@nya.route("/addtask", methods=['POST'])
def addtask():
    try:
        data = request.get_json()
        category = data.get('category', 'todo').lower()  # Default to 'todo'
        label = data('label')
        date = data('date')

        if not label or not date:
            return jsonify({"status": "error", "message": "Fields cannot be empty"})
        
        if category not in ['todo', 'doing', 'done']:
            return jsonify({"status": "error", "message": "Invalid category"})

        # Save the task to a file
        with open(TASK_FILE, "a") as file:
            file.write(f"{label},{date}\n")
            return jsonify({"message": "Task added successfully", "task": label})
    
    except Exception as e:
        return jsonify({"message": "An error occurred", "error": str(e)})

@nya.route("/todo", methods=['GET'])
def todo():
    """
    Fetch all tasks categorized as 'To-Do'
    """
    return fetch_tasks_by_category('todo')


@nya.route("/doing", methods=['GET'])
def doing():
    """
    Fetch all tasks categorized as 'Doing'
    """
    return fetch_tasks_by_category('doing')


@nya.route("/done", methods=['GET'])
def done():
    """
    Fetch all tasks categorized as 'Done'
    """
    return fetch_tasks_by_category('done')

@nya.route("/tasks", methods=['GET'])
def get_all_tasks():
    """
    Fetch all tasks regardless of category
    """
    try:
        tasks = []
        with open(TASK_FILE, "r") as file:
            for line in file:
                category, label, date = line.strip().split(",")
                tasks.append({"category": category, "label": label, "date": date})

        return jsonify({"status": "success", "tasks": tasks})

    except FileNotFoundError:
        return jsonify({"status": "error", "message": "No tasks found"})

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)})

@nya.route("/change_category", methods=['POST'])
def change_category():
    """
    Change the category of an existing task
    """
    try:
        data = request.get_json()
        label = data.get('label')
        new_category = data.get('new_category', 'todo').lower()

        if not label or not new_category:
            return jsonify({"status": "error", "message": "Fields cannot be empty"}), 400

        if new_category not in ['todo', 'doing', 'done']:
            return jsonify({"status": "error", "message": "Invalid new category"}), 400

        # Read tasks from the file and update the category
        tasks = []
        task_found = False
        with open(TASK_FILE, "r") as file:
            for line in file:
                category, task_label, date = line.strip().split(",")
                if task_label == label:
                    tasks.append(f"{new_category},{task_label},{date}\n")
                    task_found = True
                else:
                    tasks.append(line)

        if not task_found:
            return jsonify({"status": "error", "message": "Task not found"}), 404

        # Write updated tasks back to the file
        with open(TASK_FILE, "w") as file:
            file.writelines(tasks)

        return jsonify({"status": "success", "message": "Task category updated successfully", "task": label, "new_category": new_category}), 200

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500


def fetch_tasks_by_category(category):
    """
    Fetch tasks from the file for a specific category
    """
    try:
        tasks = []
        with open(TASK_FILE, "r") as file:
            for line in file:
                task_category, label, date = line.strip().split(",")
                if task_category == category:
                    tasks.append({"label": label, "date": date})

        return jsonify({"status": "success", "tasks": tasks})

    except FileNotFoundError:
        return jsonify({"status": "error", "message": "No tasks found"})

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)})
