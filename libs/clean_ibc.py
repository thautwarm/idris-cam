from pathlib import Path
from os import remove
                 

p = Path(".")
def clean(p):
    for each in p.iterdir():
        p: Path
        if p.is_dir():
            clean(p)
        elif p.suffix() == '.ibc':
            remove(str(p))
clean(p)
        
        
                  
        
