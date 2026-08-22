<h1>Pima Diabetes Data Analysis</h1>

<h2>1. Overview</h2>

<p>
This project performs exploratory data analysis (EDA) and logistic regression analysis
using the <strong>Pima Diabetes dataset</strong>.
</p>

<p>The main objectives are:</p>

<ul>
  <li>To explore the characteristics of the dataset.</li>
  <li>To identify and handle zero values that may represent missing observations.</li>
  <li>To investigate relationships between explanatory variables and diabetes outcome.</li>
  <li>To create a binary variable, <code>ThreeOrMoreKids</code>, based on the number of pregnancies.</li>
  <li>To perform simple logistic regression.</li>
  <li>To perform multiple logistic regression and compare different models using AIC.</li>
  <li>To use the selected model to predict diabetes probabilities for new observations in <code>ToPredict.csv</code>.</li>
  <li>To compare predictions before and after replacing zero values with column means.</li>
</ul>

<hr>

<h2>2. Dataset</h2>

<p>
The main dataset is:
</p>

<p><code>PimaDiabetes.csv</code></p>

<p>
The dataset contains 750 observations and 9 variables.
</p>

<table>
  <thead>
    <tr>
      <th>Variable</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><code>Pregnancies</code></td>
      <td>Number of times pregnant</td>
    </tr>
    <tr>
      <td><code>Glucose</code></td>
      <td>Plasma glucose concentration</td>
    </tr>
    <tr>
      <td><code>BloodPressure</code></td>
      <td>Diastolic blood pressure</td>
    </tr>
    <tr>
      <td><code>SkinThickness</code></td>
      <td>Triceps skin fold thickness</td>
    </tr>
    <tr>
      <td><code>Insulin</code></td>
      <td>2-Hour serum insulin</td>
    </tr>
    <tr>
      <td><code>BMI</code></td>
      <td>Body mass index</td>
    </tr>
    <tr>
      <td><code>DiabetesPedigree</code></td>
      <td>Diabetes pedigree function</td>
    </tr>
    <tr>
      <td><code>Age</code></td>
      <td>Age</td>
    </tr>
    <tr>
      <td><code>Outcome</code></td>
      <td>Diabetes outcome (0 = no diabetes, 1 = diabetes)</td>
    </tr>
  </tbody>
</table>

<p>
A separate file, <code>ToPredict.csv</code>, is used for prediction.
</p>

<h2>3. Required R Packages</h2>

<p>The following R packages are required:</p>

<pre><code>install.packages("dplyr")
install.packages("moments")
install.packages("MCMCpack")
install.packages("GGally")
install.packages("VIM")</code></pre>

<p>The libraries are loaded using:</p>

<pre><code>library(plyr)
library(dplyr)
library(ggplot2)
library(GGally)
library(VIM)
library(moments)</code></pre>

<h2>4. Working Directory</h2>

<p>
The working directory should contain the CSV files.
The path needs to be changed depending on the user's computer.
</p>

<pre><code>setwd("C:/Users/Naito/Documents/MScDataSciense/2.Statistic and Machine Learning 1 DATA70121/★assinment_12Nov1700")</code></pre>

<p>
The current working directory can be checked using:
</p>

<pre><code>getwd()</code></pre>

<hr>

<h2>5. Exploratory Data Analysis</h2>

<h3>5.1 Reading the Data</h3>

<p>
The dataset is loaded using:
</p>

<pre><code>auto.data &lt;- read.csv(
  "PimaDiabetes.csv",
  header = TRUE,
  na.strings = c("")
)</code></pre>

<p>
Basic information is examined using:
</p>

<pre><code>head(auto.data)
summary(auto.data)
dim(auto.data)</code></pre>

<p>
The dataset contains:
</p>

<ul>
  <li>750 rows</li>
  <li>9 columns</li>
</ul>

<h3>5.2 Checking Missing Values</h3>

<p>
Missing values are checked using:
</p>

<pre><code>is.na(auto.data)
auto.data[!complete.cases(auto.data), ]</code></pre>

<p>
There are no explicit missing values in the original dataset.
However, some variables contain zero values that may represent missing observations.
</p>

<p>
In particular, zero values in the following variables are treated as missing:
</p>

<ul>
  <li><code>Glucose</code></li>
  <li><code>BloodPressure</code></li>
  <li><code>SkinThickness</code></li>
  <li><code>Insulin</code></li>
  <li><code>BMI</code></li>
</ul>

<h2>6. Replacing Zero Values</h2>

<p>
A copy of the original dataset is created:
</p>

<pre><code>re0na &lt;- auto.data</code></pre>

<p>
Zero values in columns 2 to 6 are converted to <code>NA</code>:
</p>

<pre><code>re0na[, 2:6][re0na[, 2:6] == 0] &lt;- NA</code></pre>

<p>
The missing values are then replaced by the mean of the corresponding variable.
For example:
</p>

<pre><code>re0na$Glucose[is.na(re0na$Glucose)] &lt;-
  mean(re0na$Glucose, na.rm = TRUE)</code></pre>

<p>
The same procedure is applied to:
</p>

<ul>
  <li><code>BloodPressure</code></li>
  <li><code>SkinThickness</code></li>
  <li><code>Insulin</code></li>
  <li><code>BMI</code></li>
</ul>

<p>
The resulting dataset is called <code>re0na</code>.
</p>

<h2>7. Visualisation</h2>

<h3>7.1 Missing Data Visualisation</h3>

<p>
The <code>VIM</code> package is used to visualise missing values:
</p>

<pre><code>vim_plot &lt;- aggr(
  auto.data,
  col = c("navyblue", "red"),
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(auto.data),
  cex.axis = 0.5,
  gap = 1.0,
  ylab = c("Histogram of missing data", "Pattern")
)</code></pre>

<h3>7.2 Scatterplot Matrix</h3>

<p>
A scatterplot matrix is produced using:
</p>

<pre><code>pairs(
  ~Pregnancies + Glucose + BloodPressure + SkinThickness +
    Insulin + BMI + DiabetesPedigree + Age + Outcome,
  data = re0na,
  main = "Scatterplot Matrices"
)</code></pre>

<p>
<code>GGally</code> is also used:
</p>

<pre><code>ggpairs(re0na)</code></pre>

<p>
These plots are used to examine relationships between the variables.
</p>

<h2>8. Correlation Analysis</h2>

<p>
The correlation between each explanatory variable and <code>Outcome</code> is calculated.
</p>

<p>For the original data:</p>

<pre><code>cor(auto.data, auto.data$Outcome)</code></pre>

<p>For the data after replacing zero values:</p>

<pre><code>cor(re0na, re0na$Outcome)</code></pre>

<p>
The correlations after replacement are approximately:
</p>

<table>
  <thead>
    <tr>
      <th>Variable</th>
      <th>Correlation with Outcome</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Pregnancies</td>
      <td>0.229</td>
    </tr>
    <tr>
      <td>Glucose</td>
      <td>0.487</td>
    </tr>
    <tr>
      <td>BloodPressure</td>
      <td>0.162</td>
    </tr>
    <tr>
      <td>SkinThickness</td>
      <td>0.217</td>
    </tr>
    <tr>
      <td>Insulin</td>
      <td>0.211</td>
    </tr>
    <tr>
      <td>BMI</td>
      <td>0.309</td>
    </tr>
    <tr>
      <td>DiabetesPedigree</td>
      <td>0.171</td>
    </tr>
    <tr>
      <td>Age</td>
      <td>0.233</td>
    </tr>
  </tbody>
</table>

<p>
<code>Glucose</code> has the strongest correlation with <code>Outcome</code>,
followed by <code>BMI</code>.
</p>

<p>
The correlation matrices are also visualised using:
</p>

<pre><code>ggcorr(auto.data)
ggcorr(re0na)</code></pre>

<h2>9. Distribution Analysis</h2>

<p>
For each numerical variable, several descriptive statistics are calculated:
</p>

<ul>
  <li>Mean</li>
  <li>Unbiased variance</li>
  <li>Biased variance</li>
  <li>Skewness</li>
  <li>Kurtosis</li>
  <li>Sample size</li>
</ul>

<p>
For example, for <code>Pregnancies</code>:
</p>

<pre><code>x1 &lt;- re0na[[1]]

mean(x1)
var(x1)

moment(x1, order = 2, central = TRUE)

ss &lt;- sqrt(moment(x1, order = 2, central = TRUE))

moment(x1, order = 3, central = TRUE) / (ss^3)

(moment(x1, order = 4, central = TRUE) / (ss^4)) - 3</code></pre>

<p>
The distributions are examined using:
</p>

<ul>
  <li>Jitter plots</li>
  <li>Histograms</li>
  <li>Histograms with density curves</li>
  <li>Boxplots</li>
</ul>

<h2>10. Creating <code>ThreeOrMoreKids</code></h2>

<p>
A new binary variable called <code>ThreeOrMoreKids</code> is created based on
the number of pregnancies.
</p>

<ul>
  <li><code>1</code> if <code>Pregnancies &gt;= 3</code></li>
  <li><code>0</code> if <code>Pregnancies &lt; 3</code></li>
</ul>

<pre><code>add.data &lt;- mutate(
  re0na,
  ThreeOrMoreKids = ifelse(Pregnancies &gt;= 3, 1, 0)
)</code></pre>

<p>
This produces a dataset with 10 columns.
</p>

<h2>11. Simple Logistic Regression</h2>

<p>
A simple logistic regression model is fitted to investigate the relationship
between diabetes outcome and <code>ThreeOrMoreKids</code>.
</p>

<pre><code>model1 &lt;- glm(
  Outcome ~ ThreeOrMoreKids,
  data = add.data,
  family = binomial
)</code></pre>

<p>
The model summary is obtained using:
</p>

<pre><code>summary(model1)</code></pre>

<p>
The coefficients are converted to odds ratios using:
</p>

<pre><code>exp(coef(model1))</code></pre>

<p>
The logistic probability is calculated using:
</p>

<pre><code>probability =
  exp(linear_predictor) /
  (1 + exp(linear_predictor))</code></pre>

<p>
The probability of diabetes is calculated separately for
<code>ThreeOrMoreKids = 0</code> and <code>ThreeOrMoreKids = 1</code>.
</p>

<h2>12. Multiple Logistic Regression</h2>

<p>
Multiple logistic regression models are fitted using different combinations
of explanatory variables.
</p>

<p>
The purpose is to identify an appropriate model for predicting
<code>Outcome</code>.
</p>

<p>
Several models are considered:
</p>

<ul>
  <li><code>model2</code></li>
  <li><code>model3</code></li>
  <li><code>model4</code></li>
  <li><code>model5</code></li>
  <li><code>model6</code></li>
  <li><code>model7</code></li>
  <li><code>model8</code></li>
  <li><code>model9</code></li>
</ul>

<p>
For example:
</p>

<pre><code>model4 &lt;- glm(
  Outcome ~ .,
  data = dat4,
  family = binomial
)</code></pre>

<p>
Each model is examined using:
</p>

<pre><code>summary(model4)</code></pre>

<h2>13. Model Selection Using AIC</h2>

<p>
The Akaike Information Criterion (AIC) is used to compare the models:
</p>

<pre><code>AIC(
  model2,
  model3,
  model4,
  model5,
  model6,
  model7,
  model8,
  model9
)</code></pre>

<table>
  <thead>
    <tr>
      <th>Model</th>
      <th>AIC</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>model2</td>
      <td>721.3241</td>
    </tr>
    <tr>
      <td>model3</td>
      <td>716.6202</td>
    </tr>
    <tr>
      <td><strong>model4</strong></td>
      <td><strong>715.4831</strong></td>
    </tr>
    <tr>
      <td>model5</td>
      <td>722.1954</td>
    </tr>
    <tr>
      <td>model6</td>
      <td>828.0721</td>
    </tr>
    <tr>
      <td>model7</td>
      <td>859.5257</td>
    </tr>
    <tr>
      <td>model8</td>
      <td>715.7630</td>
    </tr>
    <tr>
      <td>model9</td>
      <td>721.6478</td>
    </tr>
  </tbody>
</table>

<p>
A lower AIC indicates a better trade-off between model fit and model complexity.
</p>

<p>
Based on the AIC comparison, <code>model4</code> is selected because it has
the lowest AIC among the models considered.
</p>

<h2>14. Prediction Using <code>ToPredict.csv</code></h2>

<p>
The file <code>ToPredict.csv</code> contains five new observations for prediction.
</p>

<p>
The data are first loaded without replacing zero values:
</p>

<pre><code>pre1 &lt;- read.csv("ToPredict.csv", header = TRUE)</code></pre>

<p>
Predictions are generated using the selected model:
</p>

<pre><code>predictData4 &lt;- predict(model4, pre1)</code></pre>

<p>
The default output from <code>predict()</code> is on the log-odds (logit) scale.
The log-odds are converted to probabilities using:
</p>

<pre><code>odds &lt;- exp(predictData4)

probability &lt;- odds / (1 + odds)</code></pre>

<p>
The resulting predicted probabilities are approximately:
</p>

<table>
  <thead>
    <tr>
      <th>Observation</th>
      <th>Predicted Probability</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td>
      <td>50.9%</td>
    </tr>
    <tr>
      <td>2</td>
      <td>30.8%</td>
    </tr>
    <tr>
      <td>3</td>
      <td>9.8%</td>
    </tr>
    <tr>
      <td>4</td>
      <td>78.3%</td>
    </tr>
    <tr>
      <td>5</td>
      <td>74.2%</td>
    </tr>
  </tbody>
</table>

<h2>15. Prediction After Replacing Zero Values</h2>

<p>
A second prediction analysis is performed after treating zero values in
<code>ToPredict.csv</code> as missing values.
</p>

<pre><code>pre2 &lt;- pre1

pre2[, 2:6][pre2[, 2:6] == 0] &lt;- NA</code></pre>

<p>
The missing values are replaced using the means calculated from the
training dataset <code>re0na</code>.
</p>

<p>For example:</p>

<pre><code>pre2$Glucose[is.na(pre2$Glucose)] &lt;-
  mean(re0na$Glucose, na.rm = TRUE)</code></pre>

<p>
The same procedure is applied to:
</p>

<ul>
  <li><code>BloodPressure</code></li>
  <li><code>SkinThickness</code></li>
  <li><code>Insulin</code></li>
  <li><code>BMI</code></li>
</ul>

<p>
The selected model is then applied:
</p>

<pre><code>predictData4 &lt;- predict(model4, pre2)</code></pre>

<p>
The resulting probabilities are:
</p>

<table>
  <thead>
    <tr>
      <th>Observation</th>
      <th>Before Replacement</th>
      <th>After Replacement</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td>
      <td>50.9%</td>
      <td>51.5%</td>
    </tr>
    <tr>
      <td>2</td>
      <td>30.8%</td>
      <td>30.8%</td>
    </tr>
    <tr>
      <td>3</td>
      <td>9.8%</td>
      <td>8.8%</td>
    </tr>
    <tr>
      <td>4</td>
      <td>78.3%</td>
      <td>78.3%</td>
    </tr>
    <tr>
      <td>5</td>
      <td>74.2%</td>
      <td>72.0%</td>
    </tr>
  </tbody>
</table>

<p>
The differences occur because zero values in the prediction dataset are
replaced by the corresponding means calculated from the training data.
</p>

<h2>16. Main Findings</h2>

<p>
The exploratory analysis indicates that <code>Glucose</code> has the strongest
positive correlation with diabetes outcome.
</p>

<p>
<code>BMI</code> and <code>Pregnancies</code> also show positive relationships
with diabetes outcome.
</p>

<p>
Several multiple logistic regression models were compared using AIC.
Among the models tested, <code>model4</code> produced the lowest AIC
(<strong>715.4831</strong>) and was therefore selected as the final model
for prediction.
</p>

<p>
The final model produced predicted diabetes probabilities ranging from
approximately <strong>8.8%</strong> to <strong>78.3%</strong> for the five
observations in <code>ToPredict.csv</code>, depending on how zero values
were handled.
</p>

<h2>17. Files</h2>

<p>
The project requires the following files:
</p>

<pre><code>PimaDiabetes.csv
ToPredict.csv
README.md</code></pre>

<p>
The R analysis script should also be kept in the same project directory.
</p>

<h2>18. How to Run</h2>

<ol>
  <li>Install the required R packages.</li>
  <li>Open the R script in RStudio.</li>
  <li>Change the <code>setwd()</code> path to the location of the project.</li>
  <li>Make sure <code>PimaDiabetes.csv</code> and <code>ToPredict.csv</code> are in the working directory.</li>
  <li>Run the script from the beginning.</li>
  <li>Check the EDA plots and statistical output.</li>
  <li>Compare the logistic regression models using AIC.</li>
  <li>Use <code>model4</code> to generate the final predictions.</li>
</ol>

<h2>19. Notes</h2>

<p>
Zero values in <code>Glucose</code>, <code>BloodPressure</code>,
<code>SkinThickness</code>, <code>Insulin</code>, and <code>BMI</code>
are treated as missing values in the main analysis.
These values are replaced by the corresponding column means.
</p>

<p>
For prediction, two approaches are compared:
</p>

<ol>
  <li>Prediction using the original values in <code>ToPredict.csv</code>.</li>
  <li>Prediction after replacing zero values with means calculated from the training dataset.</li>
</ol>

<p>
This comparison demonstrates how the treatment of potentially missing values
can affect logistic regression predictions.
</p>

<hr>

<p>
<strong>End of README</strong>
</p>
