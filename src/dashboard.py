from . import TASK_FILE
from flask import (
    Blueprint, jsonify, request
)

nya = Blueprint('nya', __name__)

@nya.route("/addtask", methods=['POST'])
def addtask():
    try:
        data = request.get_json()
        taskid = data.get("id")
        name = data.get("name")
        desc = data.get("description")
        date = data.get("date")
        status = data.get("status")
        print(f"id:", {taskid})
        print(f"name:", {name})
        print(f"desc:", {desc})
        print(f"date:", {date})
        print(f"status:", {status})

         # Validate required fields
        if not name or not date or not status:
            return jsonify({"status": "error", "message": "Fields 'name', 'date', and 'status' cannot be empty"}), 400

        if status not in ['To do', 'Doing', 'Done']:
            return jsonify({"status": "error", "message": "Invalid status"}), 409

        # Save the task to a file (id, name, description, date, status)
        with open(TASK_FILE, "a") as file:
            file.write(f"{taskid},{name},{desc},{date},{status}\n")
        
        return jsonify({"status": "success","message": "Task added successfully", "task": {"id": taskid, "name": name, "status": status}}), 201
    
    except Exception as e:
        return jsonify({"status": "error","message": "An error occurred", "error": str(e)}), 500

@nya.route("/change_status", methods=['POST'])
def change_status():
    """
    Change the status of an existing task.
    """
    try:
        data = request.get_json()
        taskid = data.get("taskid")
        new_status = data.get("status")

        if not taskid or not new_status:
            return jsonify({"status": "error", "message": "Task ID and new status are required"}), 400

        if new_status not in ['To do', 'Doing', 'Done']:
            return jsonify({"status": "error", "message": "Invalid status"}), 400
        
        tasks = []
        task_found = False
        
        with open(TASK_FILE, "r") as file:
            for line in file:
                fields = line.strip().split(",")
                if fields[0] == taskid:  # Find the task by taskid
                    fields[4] = new_status  # Update the status field
                    task_found = True
                    tasks.append(",".join(fields) + "\n")
                else:
                    tasks.append(line)  # Keep the other tasks as they are
        
        if not task_found:
            return jsonify({"status": "error", "message": "Task not found"}), 404

        # Write updated tasks back to the file
        with open(TASK_FILE, "w") as file:
            file.writelines(tasks)
        
        return jsonify({"status": "success", "message": "Task status updated successfully", "taskid": taskid, "new_status": new_status}), 200

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500

@nya.route("/delete_task", methods=['DELETE'])
def delete_task():
    """
    Delete a task by its taskid.
    """
    try:
        data = request.get_json()
        taskid = data.get("taskid")
        
        if not taskid:
            return jsonify({"status": "error", "message": "Task ID is required"}), 400
        
        tasks = []
        task_found = False
        
        with open(TASK_FILE, "r") as file:
            for line in file:
                fields = line.strip().split(",")
                if fields[0] == taskid:  # Compare the taskid to the one in the file
                    task_found = True
                    continue  # Skip the line if it's the task we want to delete
                tasks.append(line)  # Keep all other tasks
        
        if not task_found:
            return jsonify({"status": "error", "message": "Task not found"}), 404
        
        with open(TASK_FILE, "w") as file:
            file.writelines(tasks)  # Write the remaining tasks back to the file
        
        return jsonify({"status": "success", "message": "Task deleted successfully"}), 200

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500

@nya.route("/tasks", methods=['GET'])
def get_all_tasks():
    """
    Fetch all tasks regardless of category with all fields dynamically.
    """
    try:
        tasks = []
        with open(TASK_FILE, "r") as file:
            for line in file:
                # Dynamically split each line into fields
                fields = line.strip().split(",")

                # Assuming the file has a fixed order of fields:
                # category, label, date, description, status, etc.
                task = {
                    "taskid": fields[0] if len(fields) > 0 else "N/A",
                    "name": fields[1] if len(fields) > 1 else "N/A",
                    "description": fields[2] if len(fields) > 2 else "N/A",
                    "date": fields[3] if len(fields) > 3 else "N/A",
                    "status": fields[4] if len(fields) > 4 else "N/A",
                }

                tasks.append(task)

        return jsonify({"status": "success", "tasks": tasks}), 200

    except FileNotFoundError:
        return jsonify({"status": "error", "message": "No tasks found"}), 400

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500

@nya.route("/save_mood", methods=['POST'])
def save_mood():
    try:
        data = request.get_json()
        mood = data.get("mood")
        
        if not mood:
            return jsonify({"status": "error", "message": "Mood cannot be empty"}), 400
        
        with open("MOOD-FILE.dat", "a") as file:
            file.write(f"{mood}\n")
        
        return jsonify({"status": "success", "message": "Mood saved successfully"}), 201
    
    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500
