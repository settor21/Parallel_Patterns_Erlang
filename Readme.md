# Parallel Patterns in Erlang

## A. Description of Parallel Patterns

### What is a Parallel Pattern?
A parallel pattern is a common method or strategy used to decompose a computational problem into smaller, concurrent tasks that can be executed simultaneously on multiple processors or cores. These patterns help to efficiently utilize multicore processors, improving performance and scalability. The main advantage of parallel patterns is that they allow for the distribution of work across multiple cores, reducing execution time and making full use of the available hardware resources.

### Task Parallelism
Task parallelism involves performing different tasks, either on the same or different data, concurrently. This approach is typically asynchronous, as tasks can be executed independently without needing to synchronize with each other. Task parallelism is useful for problems that can be divided into distinct subtasks, each of which can run in parallel to improve overall execution efficiency.

### 1. Map Pattern
The map pattern involves applying a given function independently to each element of a collection, such as a list or an array. This allows for task parallelism, where each element can be processed concurrently. This pattern is useful when the same operation needs to be performed on multiple data items, and there are no dependencies between these operations.

### 2. Pipeline Pattern
The pipeline pattern involves breaking down a task into a series of stages, where each stage performs a specific operation on the data and then passes the result to the next stage. This pattern is suitable for task parallelism, where different tasks are performed on the same or different data in a sequential manner. Each stage can operate concurrently, and the stages are typically connected in a linear sequence.

### Data Parallelism
Data parallelism involves performing the same task on different subsets of the same data concurrently. This approach is typically synchronous, as each subset is processed in parallel and often requires synchronization to combine results. Data parallelism is beneficial for problems that can be divided into smaller chunks of data that can be processed independently and in parallel.

### 3. Farm Pattern
The farm pattern, also known as the worker farm pattern, involves distributing tasks among multiple workers that perform the same operation independently. Each worker processes a portion of the input data, and the results are collected at the end. This pattern is ideal for embarrassingly parallel problems where tasks are completely independent and can be executed in parallel without any need for synchronization.

### 4. Reduction Pattern
The reduction pattern involves performing a reduction operation, such as summation, product, or finding the maximum, across a data set. This pattern processes different subsets of the data in parallel to combine them into a single result. It is synchronous because each subset is processed in parallel to contribute to the final result.

## B. Tasks for Each Parallel Pattern

### Task Parallelism

#### 1. Map Pattern
- **Image Blurring:** Applying a blurring filter to an image where each pixel's value is averaged with its neighbors.
- **Image Sharpening:** Enhancing the edges in an image by emphasizing the contrast between neighboring pixels.

#### 2. Pipeline Pattern
##### Image Processing Pipeline:
- **Stage 1:** Image resizing (downscaling or upscaling an image).
- **Stage 2:** Color transformation (changing the color space of the image, e.g., RGB to grayscale).

##### Data Transformation Pipeline:
- **Stage 1:** Data cleaning (removing noise or invalid entries from the dataset).
- **Stage 2:** Data normalization (scaling the data to a standard range or distribution).

### Data Parallelism

#### 3. Farm Pattern
- **Matrix Multiplication:** Performing matrix multiplication where each worker computes a portion of the result matrix.
- **Sorting Large Arrays:** Sorting large datasets using a parallel merge sort algorithm, where each worker handles a portion of the array.

#### 4. Reduction Pattern
- **Sum Reduction:** Calculating the sum of elements in a large array by dividing the array into chunks and summing each chunk in parallel.
- **Maximum Value Finding:** Finding the maximum value in a large dataset by dividing the data into chunks and determining the maximum in each chunk in parallel, then combining the results.
