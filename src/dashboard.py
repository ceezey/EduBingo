from . import TASK_FILE, MOOD_FILE
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

@nya.route("/change_status", methods=['PUT'])
def change_status():
    """
    Change the status of an existing task.
    """
    try:
        data = request.get_json()
        new_name = data.get("name")
        new_desc = data.get("description")
        new_date = data.get("date")
        new_status = data.get("status")

        # print(f"Task DATA: {new_name},{new_date},{new_desc},{new_status}")

        if new_status not in ['To do', 'Doing', 'Done']:
            return jsonify({"status": "error", "message": "Invalid status"}), 400
        
        tasks = []
        task_found = False
        
        with open(TASK_FILE, "r") as file:
            for line in file:
                fields = line.strip().split(",")
                if fields[1] == new_name:  # Find by name
                    fields[2] = new_desc  # Update the description field
                    fields[3] = new_date # Update the date field
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
        
        return jsonify({"status": "success", "message": "Task status updated successfully", "new_name": new_name, "new_status": new_status}), 200

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500

@nya.route("/delete_task", methods=['DELETE'])
def delete_task():
    """
    Delete a task by its taskname.
    """
    try:
        data = request.get_json()
        taskname = data.get("taskname")
        
        tasks = []
        task_found = False
        
        with open(TASK_FILE, "r") as file:
            for line in file:
                fields = line.strip().split(",")
                if fields[1] == taskname:  # Compare the taskid to the one in the file
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
                # print(fields)
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
        date = data.get("date")
        mood = data.get("mood")

        print(date, mood)

        if not mood:
            return jsonify({"status": "error", "message": "Mood cannot be empty"}), 400
        
        with open(MOOD_FILE, "a") as file:
            file.write(f"{date},{mood},false\n")
        
        return jsonify({"status": "success", "message": "Mood saved successfully"}), 201
    
    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500
    
@nya.route("/show_again", methods=['GET'])
def showAgain():
    try:
        # Read the last saved "showAgain" value from the file
        with open(MOOD_FILE, "r") as file:
            lines = file.readlines()

        # If the file is empty, default to True
        if not lines:
            return jsonify({"showAgain": True}), 200

        # Parse the last entry in the file
        last_entry = lines[-1].strip().split(",")

        # Ensure the last entry has at least 3 fields (date, mood, showAgain)
        if len(last_entry) < 3:
            print(show_again_value)
            return jsonify({"showAgain": True}), 200  # Default to True if the last entry is malformed

        # Determine the value of "showAgain" from the last entry
        show_again_value = last_entry[2].lower() == "true"
        print(show_again_value)
        return jsonify({"showAgain": show_again_value}), 200

    except FileNotFoundError:
        # File not found, default to showing the modal
        return jsonify({"showAgain": True}), 200

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500
    
@nya.route("/update_show_again", methods=['PUT'])
def update_show_again():
    try:
        # Open the file and read the existing entries
        with open(MOOD_FILE, "r") as file:
            lines = file.readlines()
        
        if not lines:
            return jsonify({"status": "error", "message": "No mood data found"}), 404

        # Get the last entry and modify the "showAgain" value to True
        last_entry = lines[-1].strip().split(",")
        last_entry[2] = "true"  # Set "showAgain" to true
        lines[-1] = ",".join(last_entry) + "\n"

        # Write the updated data back to the file
        with open(MOOD_FILE, "w") as file:
            file.writelines(lines)

        return jsonify({"status": "success", "message": "showAgain updated to true"}), 200

    except FileNotFoundError:
        return jsonify({"status": "error", "message": "Mood file not found"}), 404

    except Exception as e:
        return jsonify({"status": "error", "message": "An error occurred", "error": str(e)}), 500
