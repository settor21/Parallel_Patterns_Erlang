# Parallel Patterns in Erlang

## A. Description of Parallel Patterns

### 1. Map Pattern
The map pattern involves applying a given function independently to each element of a collection, such as a list or an array. This allows for task parallelism, where each element can be processed concurrently. This pattern is useful when the same operation needs to be performed on multiple data items, and there are no dependencies between these operations.

### 2. Farm Pattern
The farm pattern, also known as the worker farm pattern, involves distributing tasks among multiple workers that perform the same operation independently. Each worker processes a portion of the input data, and the results are collected at the end. This pattern is ideal for embarrassingly parallel problems where tasks are completely independent and can be executed in parallel without any need for synchronization.

### 3. Pipeline Pattern
The pipeline pattern involves breaking down a task into a series of stages, where each stage performs a specific operation on the data and then passes the result to the next stage. This pattern is suitable for data parallelism, where data flows through a sequence of processing steps. Each stage can operate concurrently, and the stages are typically connected in a linear sequence.

### 4. Stencil Computation
Stencil computation is a data parallelism pattern used in scenarios where each point in a data grid is updated based on the values of its neighboring points. This method is common in scientific computing, image processing, and numerical simulations. It involves iteratively applying a computation over a data grid, taking into account the values of adjacent points to update each point.

## B. Tasks for Each Parallel Pattern

### 1. Map Pattern
- **Image Blurring:** Applying a blurring filter to an image where each pixel's value is averaged with its neighbors.
- **Image Sharpening:** Enhancing the edges in an image by emphasizing the contrast between neighboring pixels.

### 2. Farm Pattern
- **Matrix Multiplication:** Performing matrix multiplication where each worker computes a portion of the result matrix.
- **Sorting Large Arrays:** Sorting large datasets using a parallel merge sort algorithm, where each worker handles a portion of the array.

### 3. Pipeline Pattern
#### Image Processing Pipeline:
- **Stage 1:** Image resizing (downscaling or upscaling an image).
- **Stage 2:** Color transformation (changing the color space of the image, e.g., RGB to grayscale).

#### Data Transformation Pipeline:
- **Stage 1:** Data cleaning (removing noise or invalid entries from the dataset).
- **Stage 2:** Data normalization (scaling the data to a standard range or distribution).

### 4. Stencil Computation
- **Heat Distribution Simulation:** Calculating the temperature distribution in a given area over time based on the heat values of neighboring points.
- **Convolution in Image Processing:** Applying convolution filters to an image where each pixel is updated based on a weighted sum of its neighbors.
