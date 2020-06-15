# Read until: "Byterun"


class VMError(Exception):
    pass


class PyVM:
    def __init__(self):
        self.frames = []  # Call stack of frames
        self.frame = None  # Current frame
        self.return_val = None
        self.last_exception = None


if __name__ == "__main__":
    raise NotImplementedError
