<?xml version="1.0" encoding="utf-8"?>
<!-- Stylesheet for the auto-generation of a config file with all of the modules switched on

Copyright 2008 GENIE Project, University of Southampton
Andrew Price, $Date: 2008-01-28 18:27:31 +0000 (Mon, 28 Jan 2008) $
$Revision: 3823 $
GENIE Toolbox for Matlab -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

	<xsl:template match="node()">
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="/">
		<xsl:element name="job">
			<xsl:element name="config">
				<xsl:apply-templates/>
			</xsl:element>
		</xsl:element>
	</xsl:template>

	<xsl:template match="config">
		<xsl:for-each select="*">
			<xsl:element name="model">
				<xsl:attribute name="name">
					<xsl:value-of select="@name"/>
				</xsl:attribute>
			</xsl:element>
		</xsl:for-each>
	</xsl:template>

</xsl:stylesheet>