import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import confusion_matrix, accuracy_score

# Load the data
data = pd.read_csv("breast-cancer-wisconsin.csv", na_values="?")

# Drop rows with missing values
data.dropna(inplace=True)

# Convert the 'Diagnosis Class' to a factor data type
data["Class"] = data["Class"].map({2: "Benign", 4: "Malignant"})

# Split the data into training (70%) and testing (30%) sets
train_data, test_data = train_test_split(data, test_size=0.3, random_state=123)

# Separate the features and target variable
X_train = train_data.drop("Class", axis=1)
y_train = train_data["Class"]
X_test = test_data.drop("Class", axis=1)
y_test = test_data["Class"]

# Train the Naïve Bayes model
naive_bayes_model = GaussianNB()
naive_bayes_model.fit(X_train, y_train)

# Make predictions using the model
predictions = naive_bayes_model.predict(X_test)

# Evaluate the model's performance
conf_matrix = confusion_matrix(y_test, predictions)
print(conf_matrix)

# Calculate accuracy
accuracy = accuracy_score(y_test, predictions)
print(f"Accuracy: {accuracy * 100:.2f}%")