<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.1"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:str="http://exslt.org/strings" >
  <xsl:output indent="no" omit-xml-declaration="yes" method="text" encoding="utf-8"/>
  <xsl:strip-space elements="*"/>

  <xsl:template match="/tests">
    <xsl:text/>[<xsl:text/>
    <xsl:apply-templates select="test"/>
    <xsl:text/>]<xsl:text/>
  </xsl:template>

  <xsl:template match="test">
    <xsl:text>{"ietId":</xsl:text>
    <xsl:value-of select="@id"/>
    <xsl:text>,"ietAddr":"</xsl:text>
    <xsl:value-of select="str:encode-uri(./address/text(), true)"/>
    <xsl:text>","ietCat":"</xsl:text>
    <xsl:value-of select="./category/text()"/>
    <xsl:text>","ietDia":"</xsl:text>
    <xsl:value-of select="./diagnosis/text()"/>
    <xsl:text>"}</xsl:text>
    <xsl:if test="following::test">
      <xsl:text>,</xsl:text>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
