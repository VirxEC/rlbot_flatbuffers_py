## rlbot-flatbuffers-py

A Python module implemented in Rust for serializing and deserializing RLBot's flatbuffers

### The goal of this project

To provide a fast, safe, and easy to use Python module for serializing and deserializing RLBot's flatbuffers.

A majority of the code is generated in the `build.rs` upon first compile and thrown into `src/`.

This includes the code generated by `flac`, the Python wrapper binds to the generated Rust code, and the Python type hints.
