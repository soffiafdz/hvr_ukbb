#!/usr/bin/env python3

import os
from pathlib import Path
import numpy as np
import csv
from tqdm import tqdm
from concurrent.futures import ThreadPoolExecutor, as_completed

def extract_matrix_from_xfm(xfm_file):
    """
    Extract the 4x4 transformation matrix from the .xfm file.

    Args:
        xfm_file (str): Path to the .xfm file.

    Returns:
        np.ndarray: a 4x4 transformation matrix.
    """
    with open(xfm_file, 'r') as file:
        lines = file.readlines()

    # Find the line containing "Linear_Transform"
    # and extract the following three lines
    for i, line in enumerate(lines):
        if "Linear_Transform" in line:
            # Read the next 3 lines containing the matrix
            matrix_lines = lines[i+1:i+4]
            matrix = []
            for matrix_line in matrix_lines:
                # Convert the values in the line into a list of floats
                matrix.append([
                    float(x.replace(';', ''))
                    for x in matrix_line.split()
                ])
            # Return the 4x4 matrix as a NumPy array
            return np.array(matrix)

def compute_distance(matrix1, matrix2):
    """
    Compute the Frobenius norm
    (a measure of the difference between the two matrices)

    Args:
        matrix1 (np.ndarray): The first 4x4 transformation matrix.
        matrix2 (np.ndarray): The second 4x4 transformation matrix.

    Returns:
        float: The Frobenius norm.

    Raises:
        ValueError: If the two matrices have different shapes.
    """
    # Ensure that the shapes of the matrices match before the comparison
    if matrix1.shape != matrix2.shape:
        raise ValueError(
            "Matrices have different shapes and cannot be compared."
        )

    # Compute the difference between the two matrices
    difference = matrix1 - matrix2

    # Return the Frobenius norm of the difference matrix
    return np.linalg.norm(difference, 'fro')


def process_subject_dir(subj_dir):
    """
    Process a subject directory to compare the xfm files inside
    by computing the distance between them.

    Args:
        subj_dir (str): The path to the subject directory containing
        the two xfm files.

    Returns:
        tuple: A tuple containing the subject's EID and the distance between
        the two transformation files.

    Raises:
        Exception: If there are issues reading or processing the .xfm files.
    """
    xfm_files = list(Path(subj_dir).glob("*.xfm"))

    # Ensure that exactly two .xfm files are present
    if len(xfm_files) != 2:
        print(
            f"Warning: Subject {subj_dir.name} "
            f"does not have exactly two .xfm files."
        )
        return None


    try:
        # Extract the transformation matrices from the two .xfm files
        matrix1 = extract_matrix_from_xfm(xfm_files[0])
        matrix2 = extract_matrix_from_xfm(xfm_files[1])

        # Compute the distance (Frobenius norm) between them
        distance = compute_distance(matrix1, matrix2)
        return (Path(subj_dir).name, distance)

    except Exception as e:
        print(f"Error extracting matrix for {subj_dir.name}: {e}")
        return None


# Define the source directory
code_dir = Path(__file__).resolve().parent
xfm_dir = code_dir.parent / 'data' / 'pp-xfms'

# Ensure that the directory with the xfms exists
if not xfm_dir.exists():
    raise FileNotFoundError(
        f"Directory with transformations not found: {xfm_dir}"
    )

# Get all subject directories
subj_dirs = [entry.path for entry in os.scandir(xfm_dir) if entry.is_dir()]

# Initialize tqdm progress bar with the total number of subject directories
with tqdm(total=len(subj_dirs),
          desc="Comparing subjects",
          unit="subjects") as pbar:
    # Process directories in parallel using ThreadPoolExecutor
    with ThreadPoolExecutor() as executor:
        futures = {executor.submit(process_subject_dir, subj_dir):
                   subj_dir for subj_dir in subj_dirs}

        # Use as_complete to update the progress bar as futures complete
        results = []
        for future in as_completed(futures):
            result = future.result()
            if result is not None:
                results.append(result)
            # Update the progress bar as each directory is processed
            pbar.update(1)

# Filter out any None results and write them to CSV
outcsv = code_dir.parent / 'data' / 'xfm_distances_reza_vlad-lng.csv'
results = [r for r in results if r is not None]

with outcsv.open('w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['EID', 'Distance'])
    for eid, distance in tqdm(results,
                              desc="Writing CSV",
                              unit="rows",
                              total=len(results),
                              ):
        writer.writerow([eid, distance])
