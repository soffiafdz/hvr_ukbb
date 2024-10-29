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
            # Add homogeneous coordinates adjustment
            matrix.append([0, 0, 0, 1])
            # Return the 4x4 matrix as a NumPy array
            return np.array(matrix)


def apply_xfm(cube, xfm):
    """
    Apply the transformation to the cube corners.

    Args:
        cube (np.ndarray): The coordinates of the cube's corners
            (in homogeneous form).
        xfm (np.ndarray): The transformation matrix (4x4).

    Returns:
        np.ndarray: Transformed coordinates after applying the transformation.
    """
    # Apply the transformation
    transformed_cube = np.dot(cube, xfm.T)
    return(transformed_cube)


def compute_displacement(orig_cube, final_cube):
    """
    Compute the Euclidean displacement between the original and transformed
    cube corners.

    Args:
        orig_cube (np.ndarray): Original 3D coordinates of the cube corners.
        final_cube (np.ndarray): Final 3D coordinates after transformations.

    Returns:
        np.ndarray: Array of displacements for each corner.
    """
    # Ensure that the shapes of the matrices match before the comparison
    return np.linalg.norm(orig_cube[:, :3] - final_cube[:, :3], axis=1)


# TODO: fix the function description
def process_subject_dir(subj_dir):
    """
    Process a subject directory to compare the xfm files inside
    by computing the displacement of an arbitrary cube transformed by them.

    Args:
        subj_dir (str): The path to the subject directory containing
        the two xfm files.

    Returns:
        tuple: A tuple containing the subject's EID and
            an array (8x1) of displacements for each cube's corner.

    Raises:
        Exception: If there are issues reading or processing the .xfm files.
    """
    xfm_files = [xfm for xfm in Path(subj_dir).glob("*.xfm")
                 if '_inv' not in xfm.name]

    # Ensure that exactly two .xfm files are present
    if len(xfm_files) != 2:
        print(
            f"Warning: Subject {Path(subj_dir).name} "
            f"does not have exactly two .xfm files."
        )
        return None

    cube_init = np.array([
        [-100, -100, -100, 1],
        [ 100, -100, -100, 1],
        [-100,  100, -100, 1],
        [ 100,  100, -100, 1],
        [-100, -100,  100, 1],
        [ 100, -100,  100, 1],
        [-100,  100,  100, 1],
        [ 100,  100,  100, 1]
    ])

    try:
        # Extract the transformation matrices from the two .xfm files
        # 0: Reza's (Ntv -> Stx); 1: Vlad's (Stx -> Ntv)
        # Louis said: send from stx -> ntv -> stx
        matrix1 = extract_matrix_from_xfm(xfm_files[1])
        matrix2 = extract_matrix_from_xfm(xfm_files[0])

        # Apply transformations
        cube_middle = apply_xfm(cube_init, matrix1)
        cube_final = apply_xfm(cube_middle, matrix2)

        # Compute the displacement (Euclidean distance) between the corners
        displacement = compute_displacement(cube_init, cube_final)
        return (Path(subj_dir).name, displacement)

    except Exception as e:
        print(f"Error extracting matrix for {Path(subj_dir).name}: {e}")
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
outcsv = code_dir.parent / 'data' / 'xfm_displacements_reza_vlad-lng.csv'
results = [r for r in results if r is not None]

with outcsv.open('w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['EID'] + [f'Corner{i}' for i in range(1,9)])
    for eid, displacements in tqdm(results,
                                   desc="Writing CSV",
                                   unit="rows",
                                   total=len(results),
                                   ):
        writer.writerow([eid] + displacements.tolist())
