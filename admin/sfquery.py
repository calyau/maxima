#!/usr/bin/env python 

# sfquery.py : python library/program to query Sourceforge bug database
#              XML files.

# See the comments below the license for usage examples.

# Copyright (C) 2003  James F. Amundson
#                     <amundson at users dot sourceforge dot net>

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.

# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA  02111-1307, USA.


# Examples:

# Find all the bugs submitted by amundson and print summaries:

# ./sfquery.py bugs-10-02-2003.xml\
#  "artifact.get('submitted_by') == 'amundson'"\
#  "artifact.print_summary()"

# Find all the *open* bugs submitted by amundson and print summaries:

# ./sfquery.py bugs-10-02-2003.xml\
#  "artifact.get('submitted_by')=='amundson' and artifact.get('status')=='Open'"\
#  "artifact.print_summary()"

# Find a specific bug and print the full record:

# ./sfquery.py bugs-10-02-2003.xml\
#  "artifact.get('artifact_id') == '596204'"\
#  "artifact.print_all()"
# Find all bugs containing plot and print who submitted them and their status:

# ./sfquery.py bugs-10-02-2003.xml\
#  "contains(artifact,'plot')"\
#  "sys.stdout.write('%s\n' % artifact.get('submitted_by'));sys.stdout.write('%s\n\n' % artifact.get('status'))"


import sys
from xml.sax import make_parser, handler, saxutils
from time import asctime, gmtime
import string
import re

def contains(thing,pattern,ignore_case=1):
    if type(thing) == str or type(thing) == unicode:
        if ignore_case:
            result = re.search(pattern,thing,re.IGNORECASE)
        else:
            result = re.search(pattern,thing)
        if result:
            return 1
        else:
            return 0
    elif hasattr(thing,"contains"):
        return thing.contains(pattern,ignore_case)
    else:
        print "Warning: contains can not check",thing,"of type",type(thing)
        return 0

class Fielded:
    def __init__(self):
        self.fields = {}
    def add(self,name,content):
        if self.fields.has_key(name):
            self.fields[name] += content
        else:
            self.fields[name] = content
    def get(self,name):
        if self.fields.has_key(name):
            uni = self.fields[name]
            return uni.encode('iso-8859-1')
        else:
            return None
    def contains(self,pattern,ignore_case=1):
        for key in self.fields.keys():
            if contains(self.fields[key],pattern,ignore_case):
                return 1
        return 0
    def print_field(self,name):
        if self.fields.has_key(name):
            print name, ":",
            if string.find(name,"date") > -1:
                print asctime(gmtime(int(self.fields[name])))
            elif name == "details":
                print
                print "-------------------------------------------------"
                print self.fields[name].encode('iso-8859-1')
                print "-------------------------------------------------"
            else:
                print self.fields[name]
    def print_all(self,prefix=None):
        for key in self.fields.keys():
            if prefix:
                print prefix,
            self.print_field(key)

class History(Fielded):
    def __init__(self):
        self.fields = {}
        self.messages = []
    def contains(self,pattern,ignore_case=1):
        for message in self.messages:
            if contains(message,pattern,ignore_case):
                return 1
        return Fielded.contains(self,pattern,ignore_case)
    def print_all(self,name,prefix=None):
        Fielded.print_all(self,prefix)
        if prefix:
            print_prefix=prefix
        else:
            print_prefix=""
        sub_prefix = print_prefix + "    "
        print print_prefix,"----------- messages -----------"
        for message in self.messages:
            message.print_all(sub_prefix)
        print print_prefix,"--------------------------------"
    
class Artifact(Fielded):
    def __init__(self):
        self.fields = {}
        self.messages = []
        self.histories = []
    def print_summary(self):
        print self.get("artifact_id"),":",
        print self.get("summary")
    def contains(self,pattern,ignore_case=1):
        for message in self.messages:
            if contains(message,pattern,ignore_case):
                return 1
        for history in self.histories:
            if contains(history,pattern,ignore_case):
                return 1
        return Fielded.contains(self,pattern,ignore_case)
    def print_all(self,prefix=None):
        if prefix:
            print_prefix=prefix
        else:
            print_prefix=""
        print print_prefix,"==================== artifact ===================="
        Fielded.print_all(self,prefix)
        sub_prefix = print_prefix + "    "
        print print_prefix,"----------- histories -----------"
        for history in self.histories:
            history.print_all(sub_prefix)
        print print_prefix,"----------- messages -----------"
        for message in self.messages:
            message.print_all(sub_prefix)
        print print_prefix,"--------------------------------"
        print print_prefix,"================================================="

class Artifacts(Fielded):
    def __init__(self):
        self.fields={}
        self.artifacts=[]
        
class ArtifactGrabber(handler.ContentHandler):
    def __init__(self,warn=1):
        self.current = []
        self.artifacts = None
        self.in_named_field = 0
        self.warn = warn

    def startElement(self, name, attrs):
        if name == "artifacts":
            self.current.append(Artifacts())
        elif name == "artifact":
            self.current.append(Artifact())
        elif name == "history":
            self.current.append(History())
        elif name == "message":
            self.current.append(Fielded())
        elif name == "field":
            if len(self.current) > 0:
                if attrs.has_key("name"):
                    self.in_named_field = 1
                    self.field_name = attrs["name"]
        else:
            if self.warn:
                print "Warning: ignoring element", name

    def endElement(self, name):
        if name == "artifacts":
            self.artifacts = self.current.pop()
        elif name == "artifact":
            this = self.current.pop()
            parent = self.current[len(self.current)-1]
            parent.artifacts.append(this)
        elif name == "message":
            this = self.current.pop()
            parent = self.current[len(self.current)-1]
            parent.messages.append(this)
        elif name == "history":
            this = self.current.pop()
            parent = self.current[len(self.current)-1]
            parent.histories.append(this)
        elif name == "field":
            self.in_named_field = 0

    def characters(self, content):
        if self.in_named_field:
            self.current[len(self.current)-1].add(self.field_name,content)
            
    def endDocument(self):
        global global_artifacts
        global_artifacts = self.artifacts.artifacts

def get_artifacts(filename):
    parser = make_parser()
    parser.setContentHandler(ArtifactGrabber(warn=0))
    parser.parse(filename)
    return global_artifacts

def simple_query(artifacts,query,action):
    print "Total number of artifacts =", len(artifacts)
    matches = 0
    for artifact in artifacts:
        if eval(query):
            matches += 1
            for cmd in string.split(action,';'):
                eval(cmd)
    print "Found", matches,"matches."
         
if __name__ == "__main__":
    if len(sys.argv) != 4:
        print """Usage:
sfquery.py <filename> <query> <action>
    <filename> is an XML dump of a SourceForge bug database.
    <query> is a python command evaluating to 1 or 0.
    <action> is a python command or commands to perform on each matched
             object. Multiple command should be separated by the ';'
             character.
"""
        sys.exit(1)
    else:
        artifacts = get_artifacts(sys.argv[1])
        simple_query(artifacts,sys.argv[2],sys.argv[3])
