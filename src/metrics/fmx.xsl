<?xml version="1.0" encoding="iso-8859-1"?>
<!-- *************************************************************************** -->
<!-- * FMX - Fortran Metrics Analysis Tool                                        -->
<!-- *************************************************************************** -->
<!-- * XSLT trandofrm stylesheet                                                 -->
<!-- * Version: 3.0                                                              -->
<!-- * Author: Fran Martinez Fadrique (GMV)                                      -->
<!-- *************************************************************************** -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/">

	<xsl:variable name="nesting_good">5</xsl:variable>
	<xsl:variable name="nesting_bad">10</xsl:variable>
	<xsl:variable name="branch_good">10</xsl:variable>
	<xsl:variable name="branch_bad">20</xsl:variable>
	<xsl:variable name="comment_good">20</xsl:variable>
	<xsl:variable name="comment_bad">10</xsl:variable>


    <html lang="en">

      <!--*** HTML header -->

      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>FMX - <xsl:value-of select="*/context/path" /></title>
        <link rel="stylesheet" type="text/css" href="{*/context/path_to_root}fmx.css" />
      </head>

      <!--*** HTML body -->

      <body>

        <!--*** Header is common to all possible file combinations -->

        <table border="0" cellpadding="1" width="100%">
          <tr>
            <td valign="top" rowspan="2" style="padding-bottom: 10px; padding-left: 10px">
              <img height="90" alt="" src="{*/context/path_to_root}logo.png">
              </img>
            </td>
            <td class="title">FMX - Fortran Metrics Analysis Tool</td>
          </tr>
          <tr>
            <td class="title2">
              Version <xsl:value-of select="*/context/version"/>
            </td>
          </tr>
          <tr>
            <td class="ruler" height="3" colspan="2" />
          </tr>
        </table>


        <!--*** Section to process the root index file -->

        <xsl:for-each select="fmxroot/statistics">

          <!--*** Statistics header section -->

          <table border="0" cellpadding="1" width="95%">

            <tr>
              <td class="headerItem">Date/time : </td>
              <td class="headerValue"><xsl:value-of select="../context/time" /></td>

              <td class="headerItem">Source lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/source" /></td>
              <td class="metricGood"><xsl:value-of select="lines/source/@percent" /> %</td>

              <td class="headerItem">Minimum complexity : </td>
              <xsl:variable name="cmmin">
                <xsl:choose>
                  <xsl:when test="complexity/minimum &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/minimum > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmmintitle">
                <xsl:choose>
                  <xsl:when test="complexity/minimum &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/minimum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmmin}' title='{$cmmintitle}'><xsl:value-of select="complexity/minimum" /></td>
            </tr>

            <tr>
              <td class="headerItem">Current view : </td>
              <td class="headerValue"><xsl:value-of select="../context/project" /></td>

              <xsl:variable name="commentmin">
                <xsl:choose>
                  <xsl:when test="lines/comments/@percent &lt; $comment_bad">metricBad</xsl:when>
                  <xsl:when test="lines/comments/@percent  >= $comment_good">metricGood</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="commentmintitle">
                <xsl:choose>
                  <xsl:when test="lines/comments/@percent &lt; $comment_bad">&lt; <xsl:copy-of select="$comment_bad" />%</xsl:when>
                  <xsl:when test="lines/comments/@percent  >= $comment_good"></xsl:when>
                  <xsl:otherwise>&lt; <xsl:copy-of select="$comment_good" />%</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class="headerItem">Comment lines : </td>
              <td class='{$commentmin}'><xsl:value-of select="lines/comments" /></td>
              <td class='{$commentmin}' title='{$commentmintitle}'><xsl:value-of select="lines/comments/@percent" /> %</td>

              <td class="headerItem">Average complexity : </td>
              <xsl:variable name="cmavg">
                <xsl:choose>
                  <xsl:when test="complexity/average &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/average > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmavgtitle">
                <xsl:choose>
                  <xsl:when test="complexity/average &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/average > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmavg}' title='{$cmavgtitle}'><xsl:value-of select="complexity/average" /></td>
            </tr>

            <tr>
              <td class="headerItem">Libraries :</td>
              <td class="headerValue"><xsl:value-of select="libraries" /></td>

              <td class="headerItem">Empty lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/empty" /></td>
              <td class="metricGood"><xsl:value-of select="lines/empty/@percent" /> %</td>

              <td class="headerItem">Maximum complexity : </td>
              <xsl:variable name="cmmax">
                <xsl:choose>
                  <xsl:when test="complexity/maximum &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/maximum > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmmaxtitle">
                <xsl:choose>
                  <xsl:when test="complexity/maximum &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/maximum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmmax}' title='{$cmmaxtitle}'><xsl:value-of select="complexity/maximum" /></td>
            </tr>

            <tr>
              <td class="headerItem"></td>
              <td class="headerValue"></td>

              <td class="headerItem">Total lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/total" /></td>
              <td class="metricGood"> </td>

              <td class="headerItem" title="> {$nesting_good} acceptable;  > {$nesting_bad} not-acceptable">Minimum nesting : </td>
              <xsl:variable name="nestmin">
                <xsl:choose>
                  <xsl:when test="nesting/minimum &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/minimum > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestmintitle">
                <xsl:choose>
                  <xsl:when test="nesting/minimum &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/minimum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestmin}' title='{$nestmintitle}'><xsl:value-of select="nesting/minimum" /></td>
            </tr>

            <tr>
              <td class="headerItem"></td>
              <td class="headerValue"></td>

              <td class="headerItem"></td>
              <td class="headerValue"></td>
              <td class="headerValue"></td>

              <td class="headerItem">Average nesting : </td>
              <xsl:variable name="nestavg">
                <xsl:choose>
                  <xsl:when test="nesting/average &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/average > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestavgtitle">
                <xsl:choose>
                  <xsl:when test="nesting/average &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/average > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestavg}' title='{$nestavgtitle}'><xsl:value-of select="nesting/average" /></td>
            </tr>

            <tr>
              <td class="headerItem"></td>
              <td class="headerValue"></td>

              <td class="headerItem"></td>
              <td class="headerValue"></td>
              <td class="headerValue"></td>

              <td class="headerItem">Maximum nesting : </td>
              <xsl:variable name="nestmax">
                <xsl:choose>
                  <xsl:when test="nesting/maximum &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/maximum > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestmaxtitle">
                <xsl:choose>
                  <xsl:when test="nesting/maximum &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/maximum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestmax}' title='{$nestmaxtitle}'><xsl:value-of select="nesting/maximum" /></td>
            </tr>

            <tr>
              <td class="headerItem"></td>
              <td class="headerValue"></td>

              <td class="headerItem"> </td>
              <td class="headerValue"> </td>

              <td class="headerItem"> </td>
              <td class="headerValue"> </td>
            </tr>

            <tr>
              <td colspan="8" height="3" class="ruler" />
            </tr>

          </table>


          <!--*** Directories section -->

          <center><table border="0" cellpadding="2" cellspacing="1" width="90%">

            <tr>
              <td class="tableHead" colspan="1">Library</td>
              <td class="tableHead" colspan="4">Statistics</td>
              <td class="tableHead" colspan="4">Complexity</td>
              <td class="tableHead" colspan="3">Nesting</td>
            </tr>

            <tr>
              <td class="tableHead2">Name</td>
              <td class="tableHead2">Source</td>
              <td class="tableHead2">Comment</td>
              <td class="tableHead2">Empty</td>
              <td class="tableHead2">Total</td>
              <td class="tableHead2">Modules</td>
              <td class="tableHead2">Minimum</td>
              <td class="tableHead2">Average</td>
              <td class="tableHead2">Maximum</td>
              <td class="tableHead2">Minimum</td>
              <td class="tableHead2">Average</td>
              <td class="tableHead2">Maximum</td>
            </tr>


            <!--*** Loop on the directories; one per line in output -->

            <xsl:for-each select="../directories/directory/statistics">
              <tr>

                <!--*** Directory name with link to specific directory index -->

                <td class="indexString">
                  <a href="{../@name}/index.xml">
                    <xsl:value-of select="../@name" />
                  </a>
                </td>

                <!-- *** Numerical statistics display (percentage and lines) -->

                <td class="methodNumber">
                  <xsl:value-of select="lines/source" />
                  (<xsl:value-of select="lines/source/@percent" />%)
                </td>

                <xsl:variable name="commentminlib">
                  <xsl:choose>
                    <xsl:when test="lines/comments/@percent &lt; $comment_bad">metricBad</xsl:when>
                    <xsl:when test="lines/comments/@percent  >= $comment_good">metricGood</xsl:when>
                    <xsl:otherwise>metricAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="commentminlibtitle">
                  <xsl:choose>
                    <xsl:when test="lines/comments/@percent &lt; $comment_bad">&lt; <xsl:copy-of select="$comment_bad" />%</xsl:when>
                    <xsl:when test="lines/comments/@percent  >= $comment_good"></xsl:when>
                    <xsl:otherwise>&lt; <xsl:copy-of select="$comment_good" />%</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$commentminlib}' title='{$commentminlibtitle}'>
                  <xsl:value-of select="lines/comments" />
                  (<xsl:value-of select="lines/comments/@percent" />%)
                </td>

                <td class="methodNumber">
                  <xsl:value-of select="lines/empty" />
                  (<xsl:value-of select="lines/empty/@percent" />%)
                </td>

                <td class="methodNumber"><xsl:value-of select="lines/total" /></td>

                <td class="methodNumber"><xsl:value-of select="modules" /></td>

                <xsl:variable name="mincm">
                  <xsl:choose>
                    <xsl:when test="complexity/minimum &lt;= $branch_good">metricGood</xsl:when>
                    <xsl:when test="complexity/minimum > $branch_bad">metricBad</xsl:when>
                    <xsl:otherwise>metricAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="mincmtitle">
                  <xsl:choose>
                    <xsl:when test="complexity/minimum &lt;= $branch_good"></xsl:when>
                    <xsl:when test="complexity/minimum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$mincm}' title='{$mincmtitle}'><xsl:value-of select="complexity/minimum" /></td>

                <xsl:variable name="avgcm">
                  <xsl:choose>
                    <xsl:when test="complexity/average &lt;= $branch_good">metricGood</xsl:when>
                    <xsl:when test="complexity/average > $branch_bad">metricBad</xsl:when>
                    <xsl:otherwise>metricAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="avgcmtitle">
                  <xsl:choose>
                    <xsl:when test="complexity/average &lt;= $branch_good"></xsl:when>
                    <xsl:when test="complexity/average > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$avgcm}' title='{$avgcmtitle}'><xsl:value-of select="complexity/average" /></td>

                <xsl:variable name="maxcm">
                  <xsl:choose>
                    <xsl:when test="complexity/maximum &lt;= $branch_good">metricGood</xsl:when>
                    <xsl:when test="complexity/maximum > $branch_bad">metricBad</xsl:when>
                    <xsl:otherwise>metricAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="maxcmtitle">
                  <xsl:choose>
                    <xsl:when test="complexity/maximum &lt;= $branch_good"></xsl:when>
                    <xsl:when test="complexity/maximum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$maxcm}' title='{$maxcmtitle}'><xsl:value-of select="complexity/maximum" /></td>

                <xsl:variable name="minnest">
                  <xsl:choose>
                    <xsl:when test="nesting/minimum &lt;= $nesting_good">nestingGood</xsl:when>
                    <xsl:when test="nesting/minimum > $nesting_bad">nestingBad</xsl:when>
                    <xsl:otherwise>nestingAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="minnesttitle">
                  <xsl:choose>
                    <xsl:when test="nesting/minimum &lt;= $nesting_good"></xsl:when>
                    <xsl:when test="nesting/minimum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$minnest}' title='{$minnesttitle}'><xsl:value-of select="nesting/minimum" /></td>

                <xsl:variable name="avgnest">
                  <xsl:choose>
                    <xsl:when test="nesting/average &lt;= $nesting_good">nestingGood</xsl:when>
                    <xsl:when test="nesting/average > $nesting_bad">nestingBad</xsl:when>
                    <xsl:otherwise>nestingAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="avgnesttitle">
                  <xsl:choose>
                    <xsl:when test="nesting/average &lt;= $nesting_good"></xsl:when>
                    <xsl:when test="nesting/average > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$avgnest}' title='{$avgnesttitle}'><xsl:value-of select="nesting/average" /></td>

                <xsl:variable name="maxnest">
                  <xsl:choose>
                    <xsl:when test="nesting/maximum &lt;= $nesting_good">nestingGood</xsl:when>
                    <xsl:when test="nesting/maximum > $nesting_bad">nestingBad</xsl:when>
                    <xsl:otherwise>nestingAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="maxnesttitle">
                  <xsl:choose>
                    <xsl:when test="nesting/maximum &lt;= $nesting_good"></xsl:when>
                    <xsl:when test="nesting/maximum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$maxnest}' title='{$maxnesttitle}'><xsl:value-of select="nesting/maximum" /></td>
              </tr>

            </xsl:for-each>

          </table></center>

        </xsl:for-each>


        <!--*** Section to process a directory index file -->

        <xsl:for-each select="fmxdir/statistics">

          <!--*** Statistics header section -->

          <table border="0" cellpadding="1" width="95%">

            <tr>
              <td class="headerItem">Date/time : </td>
              <td class="headerValue"><xsl:value-of select="../context/time" /></td>

              <td class="headerItem">Source lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/source" /></td>
              <td class="metricGood"><xsl:value-of select="lines/source/@percent" /> %</td>

              <td class="headerItem">Minimum complexity : </td>
              <xsl:variable name="cmmin">
                <xsl:choose>
                  <xsl:when test="complexity/minimum &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/minimum > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmmintitle">
                <xsl:choose>
                  <xsl:when test="complexity/minimum &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/minimum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmmin}' title='{$cmmintitle}'><xsl:value-of select="complexity/minimum" /></td>
            </tr>

            <tr>
              <td class="headerItem">Current view : </td>
              <td class="headerValue">
                <a href="{../context/path_to_root}index.xml">
                  <xsl:value-of select="../context/project" />
                </a>
                &#160;-&#160;
                <xsl:value-of select="../context/path" />
              </td>

              <xsl:variable name="commentmin">
                <xsl:choose>
                  <xsl:when test="lines/comments/@percent &lt; $comment_bad">metricBad</xsl:when>
                  <xsl:when test="lines/comments/@percent  >= $comment_good">metricGood</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="commentmintitle">
                <xsl:choose>
                  <xsl:when test="lines/comments/@percent &lt; $comment_bad">&lt; <xsl:copy-of select="$comment_bad" />%</xsl:when>
                  <xsl:when test="lines/comments/@percent  >= $comment_good"></xsl:when>
                  <xsl:otherwise>&lt; <xsl:copy-of select="$comment_good" />%</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class="headerItem">Comment lines : </td>
              <td class='{$commentmin}'><xsl:value-of select="lines/comments" /></td>
              <td class='{$commentmin}' title='{$commentmintitle}'><xsl:value-of select="lines/comments/@percent" /> %</td>

              <td class="headerItem">Average complexity : </td>
              <xsl:variable name="cmavg">
                <xsl:choose>
                  <xsl:when test="complexity/average &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/average > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmavgtitle">
                <xsl:choose>
                  <xsl:when test="complexity/average &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/average > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmavg}' title='{$cmavgtitle}'><xsl:value-of select="complexity/average" /></td>
            </tr>

            <tr>
              <td class="headerItem">Files :</td>
              <td class="headerValue"><xsl:value-of select="count(files/file)" /></td>

              <td class="headerItem">Empty lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/empty" /></td>
              <td class="metricGood"><xsl:value-of select="lines/empty/@percent" /> %</td>

              <td class="headerItem">Maximum complexity : </td>
              <xsl:variable name="cmmax">
                <xsl:choose>
                  <xsl:when test="complexity/maximum &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/maximum > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmmaxtitle">
                <xsl:choose>
                  <xsl:when test="complexity/maximum &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/maximum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmmax}' title='{$cmmaxtitle}'><xsl:value-of select="complexity/maximum" /></td>
            </tr>

            <tr>
              <td class="headerItem">Modules :</td>
              <td class="headerValue"><xsl:value-of select="modules" /></td>

              <td class="headerItem">Total lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/total" /></td>
              <td class="metricGood"> </td>

              <td class="headerItem">Minimum nesting : </td>
              <xsl:variable name="nestmin">
                <xsl:choose>
                  <xsl:when test="nesting/minimum &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/minimum > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestmintitle">
                <xsl:choose>
                  <xsl:when test="nesting/minimum &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/minimum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestmin}' title='{$nestmintitle}'><xsl:value-of select="nesting/minimum" /></td>
            </tr>

            <tr>
              <td class="headerItem"></td>
              <td class="headerValue"></td>

              <td class="headerItem"></td>
              <td class="headerValue"></td>
              <td class="headerValue"></td>

              <td class="headerItem">Average nesting : </td>
              <xsl:variable name="nestavg">
                <xsl:choose>
                  <xsl:when test="nesting/average &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/average > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestavgtitle">
                <xsl:choose>
                  <xsl:when test="nesting/average &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/average > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestavg}' title='{$nestavgtitle}'><xsl:value-of select="nesting/average" /></td>
            </tr>

            <tr>
              <td class="headerItem"></td>
              <td class="headerValue"></td>

              <td class="headerItem"></td>
              <td class="headerValue"></td>
              <td class="headerValue"></td>

              <td class="headerItem">Maximum nesting : </td>
              <xsl:variable name="nestmax">
                <xsl:choose>
                  <xsl:when test="nesting/maximum &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/maximum > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestmaxtitle">
                <xsl:choose>
                  <xsl:when test="nesting/maximum &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/maximum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestmax}' title='{$nestmaxtitle}'><xsl:value-of select="nesting/maximum" /></td>
            </tr>

            <tr>
              <td class="headerItem"></td>
              <td class="headerValue"></td>

              <td class="headerItem"> </td>
              <td class="headerValue"> </td>

              <td class="headerItem"> </td>
              <td class="headerValue"> </td>
            </tr>

            <tr>
              <td colspan="8" height="3" class="ruler" />
            </tr>

          </table>


          <!--*** Files section -->

          <center><table border="0" cellpadding="2" cellspacing="1" width="90%">

            <tr>
              <td class="tableHead" colspan="2">Method</td>
              <td class="tableHead" colspan="4">Statistics</td>
              <td class="tableHead" colspan="4">Complexity</td>
              <td class="tableHead" colspan="3">Nesting</td>
            </tr>

            <tr>
              <td class="tableHead2">Name</td>
              <td class="tableHead2">Type</td>
              <td class="tableHead2">Source</td>
              <td class="tableHead2">Comment</td>
              <td class="tableHead2">Empty</td>
              <td class="tableHead2">Total</td>
              <td class="tableHead2">Methods</td>
              <td class="tableHead2">Minimum</td>
              <td class="tableHead2">Average</td>
              <td class="tableHead2">Maximum</td>
              <td class="tableHead2">Minimum</td>
              <td class="tableHead2">Average</td>
              <td class="tableHead2">Maximum</td>
            </tr>


            <!--*** Loop on the files one per line in output -->

            <xsl:for-each select="../files/file/statistics">
              <tr>

                <!--*** File name with link to specific statistics file -->

                <td class="indexString">
                  <a href="{../@name}.xml">
                    <xsl:value-of select="../@name" />
                  </a>
                </td>

                <td class="indexString">
                  <xsl:value-of select="../@type" />
                </td>

                <!-- *** File statistics -->

                <td class="indexNumber" align="right">
                  <xsl:value-of select="lines/source" />
                </td>

                <td class="indexNumber">
                  <xsl:value-of select="lines/comments" />
                </td>

                <td class="indexNumber">
                  <xsl:value-of select="lines/empty" />
                </td>

                <td class="indexNumber">
                  <xsl:value-of select="lines/total" />
                </td>

                
                <!-- *** Numerical statistics display (percentage and lines) -->

                <td class="indexNumber">
                  <xsl:value-of select="methods" />
                </td>

                <xsl:variable name="min_cm">
                  <xsl:choose>
                    <xsl:when test="complexity/minimum &lt;= $branch_good">metricGood</xsl:when>
                    <xsl:when test="complexity/minimum > $branch_bad">metricBad</xsl:when>
                    <xsl:otherwise>metricAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="min_cmtitle">
                  <xsl:choose>
                    <xsl:when test="complexity/minimum &lt;= $branch_good"></xsl:when>
                    <xsl:when test="complexity/minimum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$min_cm}' title='{$min_cmtitle}'><xsl:value-of select="complexity/minimum" /></td>

                <xsl:variable name="avg_cm">
                  <xsl:choose>
                    <xsl:when test="complexity/average &lt;= $branch_good">metricGood</xsl:when>
                    <xsl:when test="complexity/average > $branch_bad">metricBad</xsl:when>
                    <xsl:otherwise>metricAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="avg_cmtitle">
                  <xsl:choose>
                    <xsl:when test="complexity/average &lt;= $branch_good"></xsl:when>
                    <xsl:when test="complexity/average > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$avg_cm}' title='{$avg_cmtitle}'><xsl:value-of select="complexity/average" /></td>

                <xsl:variable name="max_cm">
                  <xsl:choose>
                    <xsl:when test="complexity/maximum &lt;= $branch_good">metricGood</xsl:when>
                    <xsl:when test="complexity/maximum > $branch_bad">metricBad</xsl:when>
                    <xsl:otherwise>metricAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="max_cmtitle">
                  <xsl:choose>
                    <xsl:when test="complexity/maximum &lt;= $branch_good"></xsl:when>
                    <xsl:when test="complexity/maximum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$max_cm}' title='{$max_cmtitle}'><xsl:value-of select="complexity/maximum" /></td>

                <xsl:variable name="min_nest">
                  <xsl:choose>
                    <xsl:when test="nesting/minimum &lt;= $nesting_good">nestingGood</xsl:when>
                    <xsl:when test="nesting/minimum > $nesting_bad">nestingBad</xsl:when>
                    <xsl:otherwise>nestingAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="min_nest_title">
                  <xsl:choose>
                    <xsl:when test="nesting/minimum &lt;= $nesting_good"></xsl:when>
                    <xsl:when test="nesting/minimum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$min_nest}' title='{$min_nest_title}'><xsl:value-of select="nesting/minimum" /></td>

                <xsl:variable name="avg_nest">
                  <xsl:choose>
                    <xsl:when test="nesting/average &lt;= $nesting_good">nestingGood</xsl:when>
                    <xsl:when test="nesting/average > $nesting_bad">nestingBad</xsl:when>
                    <xsl:otherwise>nestingAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="avg_nest_title">
                  <xsl:choose>
                    <xsl:when test="nesting/average &lt;= $nesting_good"></xsl:when>
                    <xsl:when test="nesting/average > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$avg_nest}' title='{$avg_nest_title}'><xsl:value-of select="nesting/average" /></td>

                <xsl:variable name="max_nest">
                  <xsl:choose>
                    <xsl:when test="nesting/maximum &lt;= $nesting_good">nestingGood</xsl:when>
                    <xsl:when test="nesting/maximum > $nesting_bad">nestingBad</xsl:when>
                    <xsl:otherwise>nestingAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="max_nest_title">
                  <xsl:choose>
                    <xsl:when test="nesting/maximum &lt;= $nesting_good"></xsl:when>
                    <xsl:when test="nesting/maximum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$max_nest}' title='{$max_nest_title}'><xsl:value-of select="nesting/maximum" /></td>

              </tr>

            </xsl:for-each>

          </table></center>

        </xsl:for-each>
 



        <!--*** Section to process an individual source file -->

        <xsl:for-each select="fmx/statistics">

          <!--*** Statistics header section -->

          <table border="0" cellpadding="1" width="95%">

            <tr>
              <td class="headerItem">Date/time : </td>
              <td class="headerValue"><xsl:value-of select="../context/time" /></td>

              <td class="headerItem">Source lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/source" /></td>
              <td class="metricGood"><xsl:value-of select="lines/source/@percent" /> %</td>

              <td class="headerItem">Minimum complexity : </td>
              <xsl:variable name="cmmin">
                <xsl:choose>
                  <xsl:when test="complexity/minimum &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/minimum > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmmintitle">
                <xsl:choose>
                  <xsl:when test="complexity/minimum &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/minimum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmmin}' title='{$cmmintitle}'><xsl:value-of select="complexity/minimum" /></td>
            </tr>

            <tr>
              <td class="headerItem">Current view : </td>
              <td class="headerValue">
                <a href="{../context/path_to_root}index.xml">
                  <xsl:value-of select="../context/project" />
    		        </a>
                &#160;-&#160;
                <a href="index.xml"><xsl:value-of select="../context/path" /></a>
                &#160;-&#160;<xsl:value-of select="../context/name" />
              </td>

              <xsl:variable name="commentmin">
                <xsl:choose>
                  <xsl:when test="lines/comments/@percent &lt; $comment_bad">metricBad</xsl:when>
                  <xsl:when test="lines/comments/@percent  >= $comment_good">metricGood</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="commentmintitle">
                <xsl:choose>
                  <xsl:when test="lines/comments/@percent &lt; $comment_bad">&lt; <xsl:copy-of select="$comment_bad" />%</xsl:when>
                  <xsl:when test="lines/comments/@percent  >= $comment_good"></xsl:when>
                  <xsl:otherwise>&lt; <xsl:copy-of select="$comment_good" />%</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class="headerItem">Comment lines : </td>
              <td class='{$commentmin}'><xsl:value-of select="lines/comments" /></td>
              <td class='{$commentmin}' title='{$commentmintitle}'><xsl:value-of select="lines/comments/@percent" /> %</td>

              <td class="headerItem">Average complexity : </td>
              <xsl:variable name="cmavg">
                <xsl:choose>
                  <xsl:when test="complexity/average &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/average > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmavgtitle">
                <xsl:choose>
                  <xsl:when test="complexity/average &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/average > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmavg}' title='{$cmavgtitle}'><xsl:value-of select="complexity/average" /></td>
            </tr>

            <tr>
              <td class="headerItem">Type : </td>
              <td class="headerValue"><xsl:value-of select="../units/unit/@type" /></td>

              <td class="headerItem">Empty lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/empty" /></td>
              <td class="metricGood"><xsl:value-of select="lines/empty/@percent" /> %</td>

              <td class="headerItem">Maximum complexity : </td>
              <xsl:variable name="cmmax">
                <xsl:choose>
                  <xsl:when test="complexity/maximum &lt;= $branch_good">metricGood</xsl:when>
                  <xsl:when test="complexity/maximum > $branch_bad">metricBad</xsl:when>
                  <xsl:otherwise>metricAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="cmmaxtitle">
                <xsl:choose>
                  <xsl:when test="complexity/maximum &lt;= $branch_good"></xsl:when>
                  <xsl:when test="complexity/maximum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$cmmax}' title='{$cmmaxtitle}'><xsl:value-of select="complexity/maximum" /></td>
            </tr>

            <tr>
              <td class="headerItem">Name : </td>
              <td class="headerValue"><xsl:value-of select="../units/unit/@name" /></td>

              <td class="headerItem">Total lines : </td>
              <td class="metricGood"><xsl:value-of select="lines/total" /></td>
              <td class="metricGood"> </td>

              <td class="headerItem" title="> {$nesting_good} acceptable;  > {$nesting_bad} not-acceptable">Minimum nesting : </td>
              <xsl:variable name="nestmin">
                <xsl:choose>
                  <xsl:when test="nesting/minimum &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/minimum > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestmintitle">
                <xsl:choose>
                  <xsl:when test="nesting/minimum &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/minimum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestmin}' title='{$nestmintitle}'><xsl:value-of select="nesting/minimum" /></td>
            </tr>

            <tr>
              <td class="headerItem">Methods : </td>
              <td class="headerValue"><xsl:value-of select="methods" /></td>

              <td class="headerItem"> </td>
              <td class="headerValue"> </td>
              <td class="headerValue"> </td>

              <td class="headerItem" title="> {$nesting_good} acceptable;  > {$nesting_bad} not-acceptable">Average nesting : </td>
              <xsl:variable name="nestavg">
                <xsl:choose>
                  <xsl:when test="nesting/average &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/average > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestavgtitle">
                <xsl:choose>
                  <xsl:when test="nesting/average &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/average > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestavg}' title='{$nestavgtitle}'><xsl:value-of select="nesting/average" /></td>
            </tr>

            <tr>
              <td class="headerItem"> </td>
              <td class="headerValue"> </td>

              <td class="headerItem"> </td>
              <td class="headerValue"> </td>
              <td class="headerValue"> </td>

              <td class="headerItem" title="> {$nesting_good} acceptable;  > {$nesting_bad} not-acceptable">Maximum nesting : </td>
              <xsl:variable name="nestmax">
                <xsl:choose>
                  <xsl:when test="nesting/maximum &lt;= $nesting_good">nestingGood</xsl:when>
                  <xsl:when test="nesting/maximum > $nesting_bad">nestingBad</xsl:when>
                  <xsl:otherwise>nestingAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <xsl:variable name="nestmaxtitle">
                <xsl:choose>
                  <xsl:when test="nesting/maximum &lt;= $nesting_good"></xsl:when>
                  <xsl:when test="nesting/maximum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                  <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>
              <td class='{$nestmax}' title='{$nestmaxtitle}'><xsl:value-of select="nesting/maximum" /></td>
            </tr>

            <tr>
              <td colspan="8" height="3" class="ruler" />
            </tr>

          </table>


          <!--*** Process the main part of the module -->

          <center><table>


             <tr>
              <td class="tableHead" colspan="2">Method</td>
              <td class="tableHead" colspan="4">Statistics</td>
              <td class="tableHead" colspan="1">Complexity</td>
              <td class="tableHead" colspan="1">Nesting</td>
            </tr>

            <tr>
              <td class="tableHead2">Name</td>
              <td class="tableHead2">Type</td>
              <td class="tableHead2">Source</td>
              <td class="tableHead2">Comment</td>
              <td class="tableHead2">Empty</td>
              <td class="tableHead2">Total</td>
              <td class="tableHead2"></td>
              <td class="tableHead2"></td>
            </tr>

           <xsl:for-each select="../units/unit/main">

              <xsl:if test="lines/total &gt; 0">

                <tr>
                  <td class="methodName"><a href="#code">main</a></td>
                  <td class="methodType"></td>

                  <td class="methodNumber">
                    <xsl:value-of select="lines/source" />
                    (<xsl:value-of select="lines/source/@percent" />%)
                  </td>
                  <td class="methodNumber">
                    <xsl:value-of select="lines/comments" />
                    (<xsl:value-of select="lines/comments/@percent" />%)
                  </td>
                  <td class="methodNumber">
                    <xsl:value-of select="lines/empty" />
                    (<xsl:value-of select="lines/empty/@percent" />%)
                  </td>
                  <td class="methodNumber"><xsl:value-of select="lines/total" /></td>

                  <td class="methodNumber"><xsl:value-of select="complexity/maximum" /></td>
                  <td class="methodNumber"><xsl:value-of select="nesting/maximum" /></td>
                </tr>

              </xsl:if>

            </xsl:for-each>


            <!--*** Loop on the methods one per line in output -->

            <xsl:for-each select="../units/unit/methods/method">
              <tr>


                <!--*** File name and type -->

                <td class="methodName">
                  <a href="#{./@name}">
                    <xsl:value-of select="./@name" />
                  </a>
                </td>
                <td class="methodType"><xsl:value-of select="./@type" /></td>


                <!-- *** Lines statistics -->

                <td class="methodNumber">
                  <xsl:value-of select="statistics/lines/source" />
                  (<xsl:value-of select="statistics/lines/source/@percent" />%)
                </td>
                <td class="methodNumber">
                  <xsl:value-of select="statistics/lines/comments" />
                  (<xsl:value-of select="statistics/lines/comments/@percent" />%)
                </td>
                <td class="methodNumber">
                  <xsl:value-of select="statistics/lines/empty" />
                  (<xsl:value-of select="statistics/lines/empty/@percent" />%)
                </td>
                <td class="methodNumber"><xsl:value-of select="statistics/lines/total" /></td>


                <!--*** Complexity statistics -->

                <xsl:variable name="branchmin">
                  <xsl:choose>
                    <xsl:when test="statistics/complexity/maximum &lt;= $branch_good">metricGood</xsl:when>
                    <xsl:when test="statistics/complexity/maximum > $branch_bad">metricBad</xsl:when>
                    <xsl:otherwise>metricAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="branchmintitle">
                  <xsl:choose>
                    <xsl:when test="statistics/complexity/maximum &lt;= $branch_good"></xsl:when>
                    <xsl:when test="statistics/complexity/maximum > $branch_bad">&gt; <xsl:copy-of select="$branch_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$branch_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$branchmin}' title='{$branchmintitle}'><xsl:value-of select="statistics/complexity/maximum" /></td>

                <!--*** Nesting statistics -->

                <xsl:variable name="nestmin_method">
                  <xsl:choose>
                    <xsl:when test="statistics/nesting/maximum &lt;= $nesting_good">nestingGood</xsl:when>
                    <xsl:when test="statistics/nesting/maximum > $nesting_bad">nestingBad</xsl:when>
                    <xsl:otherwise>nestingAccept</xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <xsl:variable name="nestmin_title">
                  <xsl:choose>
                    <xsl:when test="statistics/nesting/maximum &lt;= $nesting_good"></xsl:when>
                    <xsl:when test="statistics/nesting/maximum > $nesting_bad">&gt; <xsl:copy-of select="$nesting_bad" /></xsl:when>
                    <xsl:otherwise>&gt; <xsl:copy-of select="$nesting_good" /></xsl:otherwise>
                  </xsl:choose>
                </xsl:variable>
                <td class='{$nestmin_method}' title='{$nestmin_title}'><xsl:value-of select="statistics/nesting/maximum" /></td>

              </tr>

            </xsl:for-each>

          </table></center>

          <table border="0" cellpadding="1" width="100%">
            <tr>
              <td class="ruler" height="3" colspan="2" />
            </tr>
          </table>


          <a name="code" />

          <!--*** Process the code section -->

          <center><table border="0" cellpadding="1" width="90%">

            <tr>
              <td class="tableHead2">L.N.</td>
              <td class="tableHead2">C</td>
              <td class="tableHead2">N</td>
              <td class="tableHead2">Statement</td>
            </tr>


            <xsl:for-each select="../code/line">

              <xsl:variable name="code_nest_class">
                <xsl:choose>
                  <xsl:when test="./@n &lt;= $nesting_good">codeKeyGood</xsl:when>
                  <xsl:when test="./@n > $nesting_bad">codeKeyBad</xsl:when>
                  <xsl:otherwise>codeKeyAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>

              <xsl:variable name="stmt_nest_class">
                <xsl:choose>
                  <xsl:when test="./@n &lt;= $nesting_good">codeLineGood</xsl:when>
                  <xsl:when test="./@n > $nesting_bad">codeLineBad</xsl:when>
                  <xsl:otherwise>codeLineAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>

              <xsl:variable name="code_branch_class">
                <xsl:choose>
                  <xsl:when test="./@b &lt;= $branch_good">codeKeyGood</xsl:when>
                  <xsl:when test="./@b > $branch_bad">codeKeyBad</xsl:when>
                  <xsl:otherwise>codeKeyAccept</xsl:otherwise>
                </xsl:choose>
              </xsl:variable>

              <xsl:variable name="target">
                <xsl:choose>
                  <xsl:when test="./@t = 'S'"><xsl:value-of select="./@k" /></xsl:when>
                  <xsl:when test="./@t = 'F'"><xsl:value-of select="./@k" /></xsl:when>
                  <xsl:when test="./@t = 'B'"><xsl:value-of select="./@k" /></xsl:when>
                  <xsl:otherwise></xsl:otherwise>
                </xsl:choose>
              </xsl:variable>

              <xsl:choose>
                <xsl:when test="$target = ''">
                  <tr>
            	      <!--- td class='{$code_branch_class}'><xsl:value-of select="./@t" /></td-->
            	      <td class="codeNumber">
                        <a name='{$target}' />
                        <xsl:value-of select="./@i" />
                      </td>
            	      <td class='{$code_branch_class}'><xsl:value-of select="./@b" /></td>
            	      <td class='{$code_nest_class}'><xsl:value-of select="./@n" /></td>
            	      <td class='{$stmt_nest_class}'><xsl:value-of select="." /></td>
                  </tr>
                </xsl:when>
                <xsl:otherwise>
                  <tr>
            	      <!--- td class='codeKeyGoodDark'><xsl:value-of select="./@t" /></td-->
            	      <td class="codeNumberDark">
                        <a name='{$target}' />
                        <xsl:value-of select="./@i" />
                      </td>
            	      <td class='codeKeyGoodDark'><xsl:value-of select="./@b" /></td>
            	      <td class='codeKeyGoodDark'><xsl:value-of select="./@n" /></td>
            	      <td class='codeLineGoodDark'><xsl:value-of select="." /></td>
                  </tr>
                </xsl:otherwise>
              </xsl:choose>

            </xsl:for-each>

          </table></center>

        </xsl:for-each>
 
        <center><table border="0" cellpadding="2" cellspacing="0" width="100%">
          <tr><td height="3" /></tr>
          <tr><td class="ruler" height="3" colspan="2" /></tr>
          <tr>
            <td height="12" valign="bottom" class="headerValue2">
              <xsl:value-of select="*/context/copyright"/>
            </td>
          </tr>
        </table></center>

          
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
