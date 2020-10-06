import urllib, base64
import numpy as np
import matplotlib.pyplot as plt
from ipykernel.kernelbase import Kernel
from io import BytesIO

def _to_png(fig):
    """Return a base64-encoded PNG from a
    matplotlib figure."""
    imgdata = BytesIO()
    fig.savefig(imgdata, format='png')
    imgdata.seek(0)
    return urllib.parse.quote(base64.b64encode(imgdata.getvalue()))

_numpy_namespace = {n: getattr(np, n) for n in dir(np)}
def _parse_function(code):
    """Return a NumPy function from a
    string 'y=f(x)'."""
    return lambda x: eval(code.split('=')[1].strip(), _numpy_namespace, {'x': x})

class PlotKernel(Kernel):
    implementation = 'Plot'
    implementation_version = '1.0'
    language = 'python'  # will be used for
                         # syntax highlighting
    language_version = '3.7'
    language_info = {'name': 'plotter',
                     'mimetype': 'text/plain',
                     'extension': '.py'}
    banner = "Simple plotting"
