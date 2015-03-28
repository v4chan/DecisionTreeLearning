// Decision tree learning

// libraries
#include <iostream>
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <map>
#include <typeinfo>
#include <iomanip>
using namespace std;

// each attribute has a corresponding numerical mapping value
// 0 = K
// 1 = Na
// 2 = CL
// 3 = HCO3
// 4 = Endotoxin
// 5 = Aniongap
// 6 = PLA2
// 7 = SDH
// 8 = GLDH
// 9 = TPP
// 10 = BreathRate
// 11 = PCV
// 12 = PulseRate
// 13 = Fibrinogen
// 14 = Dimer
// 15 = FibPerDim
// 16 = classification

// tree structure
struct TreeNode {
	TreeNode * parent;
	float attribute;
	float threshold;
	TreeNode * left;
	TreeNode * right;
};

// if thresholds are the same, ensure thresholds are different from root to leaves
bool diff_threshold(TreeNode * T, float threshold, float attr) {
	if (T->parent == NULL) {
		return true;
	}
	else if (T->parent->threshold == threshold && T->parent->attribute == attr) {
		return false;
	}
	else {
		diff_threshold(T->parent, threshold, attr);
	}
}

// checks if examples are from the same class
bool same_class(vector < vector<float> > examples) {
	float tracker = examples[0][examples[0].size()-1];
	for (int i = 0; i < examples.size(); i++) {
		if (tracker != examples[i][examples[0].size()-1]) {
			return false;
		}
	}
	return true;
}
				
// information gain formula
float information_gain(float p,float n) {
	if (p == 0) { return -(n/(p+n))*log(n/(p+n)); }
	else if (n == 0) { return -(p/(p+n))*log(p/(p+n)); }
	else if (p == 0 && n == 0) { return 0; }
	else {
		return -(p/(p+n))*log(p/(p+n))-(n/(p+n))*log(n/(p+n)); 
	}

}	

// information gain calculation
float IG(vector< vector<float> > examples, int attribute, float threshold, bool flag) {
	// determines information gain, I((p/(p+n)),(n/(p+n)))
	if (flag == false) {
		float p = 0;
		float n = 0;
		int values_size = examples[0].size();
		for (int a = 0; a < examples.size(); a++) {
			if (examples[a][values_size-1] == 1) { 
				p++;
			}
			else { n++; }
		}
		return information_gain(p,n);
	}
	// determines remainder of attribute
	else {
		float p_above = 0;
		float n_above = 0;
		float p_below = 0;
		float n_below = 0;
		int values_size = examples[0].size();
		for (int b = 0; b < examples.size(); b++) {
			if (examples[b][values_size-1] == 1 && examples[b][attribute] > threshold) { p_above++; }
			else if (examples[b][values_size-1] == 1 && examples[b][attribute] <= threshold) { p_below++; }
			else if (examples[b][values_size-1] == 0 && examples[b][attribute] > threshold) { n_above++; }
			else { n_below++; }
		}
		float total = p_above + n_above + p_below + n_below;
		float above = (p_above+n_above)/total;
		float below = (p_below+n_below)/total;
		return -above*information_gain(p_above,n_above)-below*information_gain(p_below,n_below);
	}
}	

// choose attribute using information gain metric
vector<float> choose_attribute(vector<float> attributes, vector< vector<float> > examples, TreeNode * T) {
	// 2 by num_attributes matrix vector that contains the max info gain for each attribute in row 1 and the associated threshold in row 2 
	vector< vector<float> > best_options(2, vector<float>(examples[0].size()-1));
	float attributes_size = attributes.size();
	float IG_value = IG(examples,0,0,false); // determining IG((p/p+n),(n/p+n))
	for (int i = 0; i < attributes_size-1; i++) {
		vector<float> info_gain; // contains the value of all the attributes
		for (int j = 0; j < examples.size(); j++) {
			info_gain.push_back(examples[j][i]);
		}
		sort(info_gain.begin(),info_gain.end()); // sort info gain vector
		info_gain.erase(unique(info_gain.begin(),info_gain.end()),info_gain.end());
		float max = 0; // determine value that gives the highest information gain
		float max_threshold = 0; // determine associating threshold of value that gives the highest information gain
		for (int k = 0; k < info_gain.size()-1; k++) {
			float analyze_threshold = (info_gain[k] + info_gain[k+1])/2;
			float analyze = IG_value - IG(examples,i,analyze_threshold,true);
			if (analyze > max && diff_threshold(T, analyze_threshold, i)) { 
					max = analyze;
					max_threshold = analyze_threshold;
			}
		}
		best_options[0][i] = max;
		best_options[1][i] = max_threshold;
	}
	vector<float> result;
	float location_max = distance(best_options[0].begin(),max_element(best_options[0].begin(),best_options[0].end())); // finding location of info max
	result.push_back(location_max); // attribute with highest threshold
	result.push_back(best_options[1][location_max]); // threshold value
	return result;
}

// finding the mode of a vector;
float findMode(vector<float> v) {
	float max = 0;
	float most_common = -1;
	map<float,float> m;
	vector<float>::iterator vi;
	for (vi = v.begin(); vi != v.end(); ++vi) {
		m[*vi]++;
		if (m[*vi] > max) {
			max = m[*vi]; 
		    most_common = *vi;
		}
	}
	return most_common;
}


int flag = 0; // flag to indicate if the function will return default vector

// inducing a decision tree
void DTL(vector< vector<float> > examples, vector<float> attributes, vector<float> def, TreeNode * DT) {
	if (examples.size() == 0) {
		DT = NULL;
		flag = 1;
		return;
	}
	else if (same_class(examples) == true) {
		if (examples[0][examples[0].size()-1] == true) { DT->attribute = -1; } // set true leaf node to be -1
		else { DT->attribute = -2; } // set false leaf node to be -2
		DT->left = NULL;
		DT->right = NULL;
		return;
	}
	else if (attributes.size() == 0) {
		DT = NULL;
		flag = 1;
		return;
	}
	else {
		vector< float > best = choose_attribute(attributes, examples, DT);
		DT->attribute = best[0];
		DT->threshold = best[1];
		// splitting examples based on threhsold value of chosen attribute
		vector< vector<float> > examples_above;
		vector< vector<float> > examples_below;
		for (int i = 0; i < examples.size(); i++) {
			if (examples[i][best[0]] > best[1]) { examples_above.push_back(examples[i]); }
			else { examples_below.push_back(examples[i]); }
		}
		
		// Pre order traversal printing of decision tree
		/*cout << "attribute: " << best[0] << " threshold: " << best[1] << endl;
		cout << "left empty? " << same_class(examples_below) << endl;
		cout << "right empty? " << same_class(examples_above) << endl;
		if (same_class(examples_below)) {
			cout << "left leaf value " << examples_below[0][examples_below[0].size()-1] << endl;
		}
		if (same_class(examples_above)) {
			cout << "right leaf value " << examples_above[0][examples_above[0].size()-1] << endl;
		}*/

		// determining mode(examples) for new trees
		vector<float> mode_above;
		vector<float> mode_below;
		if (examples_above.size() > 0) {
			for (int c = 0; c < examples_above[0].size(); c++) {
				vector<float> temp;
				for (int d = 0; d < examples_above.size(); d++) { temp.push_back(examples_above[d][c]); }
				mode_above.push_back(findMode(temp));
			}
		}
		if (examples_below.size() > 0) {
			for (int c = 0; c < examples_below[0].size(); c++) {
				vector<float> temp;
				for (int d = 0; d < examples_below.size(); d++) { temp.push_back(examples_below[d][c]); }
				mode_below.push_back(findMode(temp));
			}
		}
		TreeNode * DT_Left = new TreeNode;
		TreeNode * DT_Right = new TreeNode;
		DT->left = DT_Left;
		DT->right = DT_Right;
		DT_Left->parent = DT;
		DT_Right->parent = DT;
		// recursively build tree until decision tree is complete
		DTL(examples_above, attributes, mode_below, DT_Right);
		DTL(examples_below, attributes, mode_above, DT_Left);
	}
}

// trace decision tree
bool trace_tree(TreeNode * T, vector<float> instance) {
	if (T->left == NULL && T->right == NULL &&(T->attribute == -1 && instance[instance.size()-1] == 1) || (T->attribute == -2 && instance[instance.size()-1] == 0)) { return true; }
	else if (T->left == NULL && T->right == NULL && (T->attribute == -1 && instance[instance.size()-1] == 0) || (T->attribute == -2 && instance[instance.size()-1] == 1)) { return false; }
	else if (T->threshold < instance[T->attribute]) { return trace_tree(T->right, instance); }
	else { return trace_tree(T->left, instance); }
}


int main () {

	// reading horseTrain.txt file and storing information in a matrix vector
	vector< vector<float> > Training_Set;
	vector<float> attributes;
	float k;
	float na;
	float cl; float hco3;
	float endo;
	float anion;
	float pla2;
	float sdh;
	float gldh;
	float tpp;
	float breath;
	float pcv;
	float pulse;
	float fib;
	float dimer;
	float fpd;
	string c;
	ifstream infile("horseTrain.txt");
	while (infile >> k >> na >> cl >> hco3 >> endo >> anion >> pla2 >> sdh >> gldh >> tpp >> breath >> pcv >> pulse >> fib >> dimer >> fpd >> c) {
		vector<float> row;
		row.push_back(k);
		row.push_back(na);
		row.push_back(cl);
		row.push_back(hco3);
		row.push_back(endo);
		row.push_back(anion);
		row.push_back(pla2);
		row.push_back(sdh);
		row.push_back(gldh);
		row.push_back(tpp);
		row.push_back(breath);
		row.push_back(pcv);
		row.push_back(pulse);
		row.push_back(fib);
		row.push_back(dimer);
		row.push_back(fpd);
		if (c == "healthy") { 
			row.push_back(1); 
		} // if example is healthy, assign a value of 1
		else {
			row.push_back(0);
		} // if example has colic, assign a value of 0
		Training_Set.push_back(row);
	}
	for (int i = 0; i < Training_Set[0].size(); i++) { attributes.push_back(i);	} // create a vector with list of attributes
	vector<float> mode;
	// finding mode(examples)
	for (int c = 0; c < Training_Set[0].size(); c++) {
		vector<float> temp;
		for (int d = 0; d < Training_Set.size(); d++) { temp.push_back(Training_Set[d][c]); }
		mode.push_back(findMode(temp));
	}
	TreeNode * DT = new TreeNode; // root of decision tree
	DT->parent = NULL;
	//cout << "Pre Order Traversal of Decision Tree" << endl;
	DTL(Training_Set,attributes,mode,DT);
	if (flag == 1) {
		cout << "Returning Default" << endl; // returns default value
	}
	else {
		cout << "Returning Decision Tree" << endl; // indicates that a decision tree has been generated by program
	}
	// pulling in test set data
	ifstream iff("horseTest.txt");
	vector < vector<float> > Testing_Set;
	while (iff >> k >> na >> cl >> hco3 >> endo >> anion >> pla2 >> sdh >> gldh >> tpp >> breath >> pcv >> pulse >> fib >> dimer >> fpd >> c) {
		vector<float> row;
		row.push_back(k);
		row.push_back(na);
		row.push_back(cl);
		row.push_back(hco3);
		row.push_back(endo);
		row.push_back(anion);
		row.push_back(pla2);
		row.push_back(sdh);
		row.push_back(gldh);
		row.push_back(tpp);
		row.push_back(breath);
		row.push_back(pcv);
		row.push_back(pulse);
		row.push_back(fib);
		row.push_back(dimer);
		row.push_back(fpd);
		if (c == "healthy") { 
			row.push_back(1); 
		} // if example is healthy, assign a value of 1
		else {
			row.push_back(0);
		} // if example has colic, assign a value of 0
		Testing_Set.push_back(row);
	}
	// determine classification of testing set
	int positive = 0;
	int negative = 0;
	for (int i = 0; i < Testing_Set.size(); i++) {
		vector<float> instance;
		instance = Testing_Set[i];
		//cout << trace_tree(DT, instance) << endl;
		if (trace_tree(DT, instance)) { 
			positive++; 
		}
		else { negative++; }
	}
	cout << "Number of Correctly Classifications: " << positive << endl;
	cout << "Number of Incorrectly Classifications: " << negative << endl;
	return 0;
}
# DecisionTreeLearning
