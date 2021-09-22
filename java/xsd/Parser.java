package xsd;

import java.io.File;
import java.io.IOException;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;


public class Parser {
	public int testXML(String xsd, String xml) {
	
		int result = 0;
		File xmlFile = null;
		File xsdFile = null;
		xmlFile = new File(xml);
		xsdFile = new File(xsd);

		if(! xmlFile.exists()) {
			System.out.println("xml file " + xmlFile.getName() + " doesn't exists");
			//System.exit(1);
			result = 1;
		}
		if(! xsdFile.exists()) {
			System.out.println("xsd file " + xsdFile.getName() + " doesn't exists");
			//System.exit(1);
			result = 2;
		}

		String extension = "";
		int i = xmlFile.getName().lastIndexOf('.');
		if (i >= 0) {
			extension = xmlFile.getName().substring(i+1);
			if(!extension.equals("xml")) {
				System.out.println(xmlFile.getName() + " not an xml file!");
				//System.exit(1);
				result = 3;
			}
		}
		extension = "";
		i = xsdFile.getName().lastIndexOf('.');
		if (i >= 0) {
			extension = xsdFile.getName().substring(i+1);
			if(!extension.equals("xsd")) {
				System.out.println(xsdFile.getName() + " not an xsd file!");
				System.exit(1);
				result = 4;
			}
		}
		Schema schema = null;
		try {
			xsdFile = new File(xsd);
			// 1. Lookup a factory for the W3C XML Schema language
			String language = "http://www.w3.org/XML/XMLSchema/v1.1";
			SchemaFactory factory = SchemaFactory.newInstance(language);

			// 2. Compile the schema.
			File schemaLocation = xsdFile;
			schema = factory.newSchema(schemaLocation);
		}
		catch (SAXException ex)
		{
			System.out.println(xsdFile.getName() + " is not valid because ");
			System.out.println(ex.getMessage());
			//System.exit(1);
			result = 5;
		}
		// 3. Get a validator from the schema.
		Validator validator = schema.newValidator();

		// 4. Parse the document you want to check.
		Source source = new StreamSource(xmlFile);
		try {
			validator.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true);
			validator.setFeature("http://apache.org/xml/features/validation/cta-full-xpath-checking", true);
		}
		catch(SAXNotRecognizedException ex) {
			System.out.println(ex.getMessage());
			//System.exit(1);
			result = 6;
		}
		catch (SAXNotSupportedException ex) {
			System.out.println(ex.getMessage());
			//System.exit(1);
			result = 7;
		}

		try {
			// 5. Check the document
			validator.validate(source);
			System.out.println(xmlFile.getName() + " is valid.");
		}
		catch (SAXException ex)
		{
			System.out.println(xmlFile.getName() + " is not valid because ");
			System.out.println(ex.getMessage());
			//System.exit(1);
			result = 8;
		}

		catch (IOException ex)
		{
			System.out.println(xmlFile.getName() + " is not valid because ");
			System.out.println(ex.getMessage());
			//System.exit(1);
			result = 9;
		}


		return result;

	}

	public static void main(String args[]){
		//int result = 0;
		if(args.length != 2) {
			System.out.println("Use this program like this: java -jar schema-check.jar xsd_file_name xml_file_name");
			System.exit(1);
		}

		Parser p = new Parser();
		int r = p.testXML(args[0], args[1]);
		System.out.println(r);
	}
}

